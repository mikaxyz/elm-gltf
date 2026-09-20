// profiler.js - opt-in WebGL profiling overlay
// ?profile=true enables and ?profile=false disables it

(function () {
    "use strict";

    var STORAGE_KEY = "profile";

    function storedEnabled() {
        try {
            return window.localStorage.getItem(STORAGE_KEY) === "true";
        } catch (err) {
            return false;
        }
    }

    function persist(enabled) {
        try {
            window.localStorage.setItem(STORAGE_KEY, enabled ? "true" : "false");
        } catch (err) {
            console.error(err);
        }
    }

    var param = new URLSearchParams(window.location.search).get("profile");
    var enabled;
    if (param === "true" || param === "false") {
        enabled = param === "true";
        persist(enabled);
    } else {
        enabled = storedEnabled();
    }

    if (!enabled) {
        return;
    }

    if (typeof Stats === "undefined") {
        console.warn("[profiler] stats.js not loaded; overlay disabled.");
        return;
    }

    var frameDrawCalls = 0;
    var frameTriangles = 0;
    var frameProgramSwitches = 0;
    var frameTextureBinds = 0;

    // GPU timing state (EXT_disjoint_timer_query). Populated lazily on the first
    // draw once we have a live context; stays disabled if the extension is absent.
    var gpu = {
        inited: false,
        available: false,
        ctx: null,
        ext: null,
        panel: null,
        pending: [],
        frameOpen: false,
        currentQuery: null,
        lastMs: null,
        max: 1
    };

    // Instrument the draw methods on both WebGL1 and WebGL2 prototypes. TRIANGLES
    // is the common case for glTF meshes; other modes still count as draw calls
    // but are not tallied into the (rough) triangle estimate.
    function trianglesFor(mode, count, ctor) {
        return mode === ctor.TRIANGLES ? count / 3 : 0;
    }

    // Lazily wire up GPU timing on the live context. WebGL1 exposes the timer via
    // EXT_disjoint_timer_query (often disabled in Chrome for Spectre, and absent in
    // Safari/Firefox) — degrade silently when it isn't there.
    function initGpuTiming(ctx) {
        gpu.inited = true;
        var ext = ctx.getExtension("EXT_disjoint_timer_query");
        if (!ext || !ext.createQueryEXT) {
            console.info("[profiler] GPU timing unavailable (EXT_disjoint_timer_query not exposed); GPU panel disabled.");
            return;
        }
        gpu.ctx = ctx;
        gpu.ext = ext;
        gpu.available = true;
        gpu.panel = stats.addPanel(new Stats.Panel("GPU", "#f0f", "#202"));
    }

    // Open a timer query around the frame's draws. Called from every draw wrapper
    // but only acts on the first draw of a frame. The microtask fires after Elm's
    // rAF callback (all synchronous draws) unwinds, so the query wraps exactly one
    // frame's worth of draw commands.
    function noteFrameStart(ctx) {
        if (!gpu.inited) {
            initGpuTiming(ctx);
        }
        if (!gpu.available || gpu.frameOpen) {
            return;
        }
        var q = gpu.ext.createQueryEXT();
        gpu.ext.beginQueryEXT(gpu.ext.TIME_ELAPSED_EXT, q);
        gpu.frameOpen = true;
        gpu.currentQuery = q;
        queueMicrotask(function () {
            if (!gpu.frameOpen) {
                return;
            }
            gpu.ext.endQueryEXT(gpu.ext.TIME_ELAPSED_EXT);
            gpu.pending.push(gpu.currentQuery);
            gpu.currentQuery = null;
            gpu.frameOpen = false;
        });
    }

    function patch(proto) {
        if (!proto) {
            return;
        }

        var drawElements = proto.drawElements;
        proto.drawElements = function (mode, count, type, offset) {
            noteFrameStart(this);
            frameDrawCalls++;
            frameTriangles += trianglesFor(mode, count, this);
            return drawElements.call(this, mode, count, type, offset);
        };

        var drawArrays = proto.drawArrays;
        proto.drawArrays = function (mode, first, count) {
            noteFrameStart(this);
            frameDrawCalls++;
            frameTriangles += trianglesFor(mode, count, this);
            return drawArrays.call(this, mode, first, count);
        };

        if (proto.drawElementsInstanced) {
            var drawElementsInstanced = proto.drawElementsInstanced;
            proto.drawElementsInstanced = function (mode, count, type, offset, instanceCount) {
                noteFrameStart(this);
                frameDrawCalls++;
                frameTriangles += trianglesFor(mode, count, this) * instanceCount;
                return drawElementsInstanced.call(this, mode, count, type, offset, instanceCount);
            };
        }

        if (proto.drawArraysInstanced) {
            var drawArraysInstanced = proto.drawArraysInstanced;
            proto.drawArraysInstanced = function (mode, first, count, instanceCount) {
                noteFrameStart(this);
                frameDrawCalls++;
                frameTriangles += trianglesFor(mode, count, this) * instanceCount;
                return drawArraysInstanced.call(this, mode, first, count, instanceCount);
            };
        }

        var useProgram = proto.useProgram;
        proto.useProgram = function (program) {
            frameProgramSwitches++;
            return useProgram.call(this, program);
        };

        var bindTexture = proto.bindTexture;
        proto.bindTexture = function (target, texture) {
            frameTextureBinds++;
            return bindTexture.call(this, target, texture);
        };
    }

    patch(window.WebGLRenderingContext && WebGLRenderingContext.prototype);
    patch(window.WebGL2RenderingContext && WebGL2RenderingContext.prototype);

    var stats = new Stats();

    var dcPanel = stats.addPanel(new Stats.Panel("DC", "#0ff", "#022"));
    var triPanel = stats.addPanel(new Stats.Panel("KTRI", "#ff0", "#220"));
    var progPanel = stats.addPanel(new Stats.Panel("PROG", "#0f0", "#020"));
    var texPanel = stats.addPanel(new Stats.Panel("TEX", "#f80", "#210"));

    var dcMax = 1;
    var triMax = 1;
    var progMax = 1;
    var texMax = 1;

    // Attach to <html>, not <body>: the examples use Browser.application, so Elm
    // owns <body> and its virtual DOM would remove anything appended there.
    stats.dom.style.position = "fixed";
    stats.dom.style.display = "flex";
    stats.dom.style.flexDirection = "column";
    stats.dom.style.top = "0";
    stats.dom.style.left = "auto";
    stats.dom.style.right = "0";
    stats.dom.style.zIndex = "1";
    document.documentElement.appendChild(stats.dom);

    // Drain completed timer queries. Results arrive a frame or two late; the panel
    // shows the most recent finished measurement. A disjoint GPU (context switch,
    // power event) invalidates in-flight queries, so drop them.
    function pollGpuTiming() {
        if (!gpu.available) {
            return;
        }
        var ext = gpu.ext;
        if (gpu.ctx.getParameter(ext.GPU_DISJOINT_EXT)) {
            gpu.pending.forEach(function (q) {
                ext.deleteQueryEXT(q);
            });
            gpu.pending = [];
            return;
        }
        var stillPending = [];
        for (var i = 0; i < gpu.pending.length; i++) {
            var q = gpu.pending[i];
            if (ext.getQueryObjectEXT(q, ext.QUERY_RESULT_AVAILABLE_EXT)) {
                var ns = ext.getQueryObjectEXT(q, ext.QUERY_RESULT_EXT);
                gpu.lastMs = ns / 1e6;
                ext.deleteQueryEXT(q);
            } else {
                stillPending.push(q);
            }
        }
        gpu.pending = stillPending;
        if (gpu.lastMs != null) {
            gpu.max = Math.max(gpu.max, gpu.lastMs);
            gpu.panel.update(gpu.lastMs, gpu.max);
        }
    }

    function tick() {
        // Counters accumulated since the previous tick represent one Elm render,
        // since both our rAF callback and Elm's fire once per frame.
        dcMax = Math.max(dcMax, frameDrawCalls);
        dcPanel.update(frameDrawCalls, dcMax);

        var kTri = frameTriangles / 1000;
        triMax = Math.max(triMax, kTri);
        triPanel.update(kTri, triMax);

        progMax = Math.max(progMax, frameProgramSwitches);
        progPanel.update(frameProgramSwitches, progMax);

        texMax = Math.max(texMax, frameTextureBinds);
        texPanel.update(frameTextureBinds, texMax);

        pollGpuTiming();

        frameDrawCalls = 0;
        frameTriangles = 0;
        frameProgramSwitches = 0;
        frameTextureBinds = 0;

        // begin()/end() with nothing between makes the FPS panel reflect the real
        // rAF cadence (the presented frame rate), which is what we want here.
        stats.begin();
        stats.end();

        requestAnimationFrame(tick);
    }

    requestAnimationFrame(tick);
})();
