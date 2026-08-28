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
                "preset": "conventionalcommits",
                "releaseRules": [
                    { "type": "docs", "release": "patch" },
                ],
            },
        ],
        "@semantic-release/release-notes-generator",
        [
            "semantic-release-mirror-version",
            {
                "fileGlob": "@(package.yaml|megaparsec-utils.cabal)",
                "placeholderRegExp": "0.0.0-dev",
            },
        ],
        [
            "@semantic-release/exec",
            {
                "prepareCmd":
                    "./scripts/prepare-release.lisp ${nextRelease.version}",
            },
        ],
        [
            "@semantic-release/git",
            {
                assets: [
                    "package.yaml",
                    "*.cabal",
                    "src/**/*.hs",
                    "api/**/*.api",
                ],
            },
        ],
        "@semantic-release/github",
        "semantic-release-stack-upload",
    ],
};
