import * as esbuild from "esbuild";
import { solidPlugin } from "esbuild-plugin-solid";
import { lessLoader } from "esbuild-plugin-less";
import { copy } from "esbuild-plugin-copy";

const ctx = await esbuild.context({
  entryPoints: ["src/index.tsx"],
  bundle: true,
  outdir: "dst",
  plugins: [
    solidPlugin(),
    lessLoader(),
    copy({
      resolveFrom: "cwd",
      assets: {
        from: ["./assets/**/*"],
        to: ["./dst"],
        watch: true,
      },
    }),
  ],
  sourcemap: true,
});

await ctx.watch();
