const { Elm } = require("./dist/elm.js");

const started = Date.now();
const app = Elm.AnimationBenchmarks.init();

const INDENT = "    ";

function buildTree(results) {
    const root = { children: new Map(), metrics: new Map() };
    for (const { name, nsPerRun } of results) {
        const group = name.slice(0, -1);
        const metric = name[name.length - 1];
        let node = root;
        for (const segment of group) {
            if (!node.children.has(segment)) {
                node.children.set(segment, { children: new Map(), metrics: new Map() });
            }
            node = node.children.get(segment);
        }
        node.metrics.set(metric, nsPerRun);
    }
    return root;
}

// current vs. benchmark as a speedup factor plus the run-time delta, e.g.
// "6.43x faster (84.4% less time)" or "1.25x slower (25.0% more time)".
function comparison(current, benchmark) {
    if (typeof current !== "number" || typeof benchmark !== "number" || current === 0 || benchmark === 0) {
        return "n/a";
    }
    const ratio = benchmark / current; // > 1 => current is faster
    const pctOfTime = (current / benchmark) * 100; // < 100 => faster, > 100 => slower
    if (Math.abs(pctOfTime - 100) < 0.05) {
        return "no change";
    }
    if (ratio >= 1) {
        return `${ratio.toFixed(2)}x faster (${pctOfTime.toFixed(1)}% of benchmark time)`;
    }
    return `${(1 / ratio).toFixed(2)}x slower (${pctOfTime.toFixed(1)}% of benchmark time)`;
}

function printNode(node, depth) {
    const pad = INDENT.repeat(depth);

    const current = node.metrics.get("current");
    const benchmark = node.metrics.get("benchmark");
    if (current !== undefined || benchmark !== undefined) {
        console.log(`${pad}current: ${current}`);
        console.log(`${pad}benchmark: ${benchmark}`);
        console.log(`${pad}change: ${comparison(current, benchmark)}`);
    }

    for (const [metric, value] of node.metrics) {
        if (metric !== "current" && metric !== "benchmark") {
            console.log(`${pad}${metric}: ${value}`);
        }
    }

    for (const [name, child] of node.children) {
        console.log(`${pad}${name}`);
        printNode(child, depth + 1);
    }
}

app.ports.sendOutput.subscribe((output) => {
    printNode(buildTree(output.results), 0);
    console.log(`\nexecution time: ${((Date.now() - started) / 1000).toFixed(1)}s`);
    if (output.warning) {
        console.log(`\nWARNING: ${output.warning}`);
    }
    process.exit(output.warning ? 1 : 0);
});
