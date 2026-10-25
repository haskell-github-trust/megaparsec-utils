/**
 * @type {import('semantic-release').GlobalConfig}
 */
export default {
  branches: ["main"],
  tagFormat: "${version}",
  plugins: [
    [
      "@semantic-release/commit-analyzer",
      {
        preset: "conventionalcommits",
        releaseRules: [{ type: "docs", release: "patch" }],
      },
    ],
    "@semantic-release/release-notes-generator",
    [
      "semantic-release-haddock-since",
      {
        versionFiles: ["package.yaml"],
        baseline: "api/megaparsec-utils.api",
      },
    ],
    [
      "@semantic-release/changelog",
      {
        changelogFile: "CHANGELOG.md",
        changelogTitle: "Changelog",
      },
    ],
    [
      "@semantic-release/git",
      {
        assets: ["package.yaml", "CHANGELOG.md", "api/*.api", "src/**/*.hs"],
      },
    ],
    "@semantic-release/github",
    "semantic-release-stack-upload",
  ],
}
