/**
 * @type {import('semantic-release').GlobalConfig}
 */
export default {
    branches: ["main"],
    tagFormat: "${version}",
    plugins: [
        "@semantic-release/commit-analyzer",
        "@semantic-release/release-notes-generator",
        [
            "semantic-release-mirror-version",
            {
                "fileGlob": "@(package.yaml|megaparsec-utils.cabal)",
                "placeholderRegExp": "0.0.0-dev"
            }
        ],
        [
            "@semantic-release/changelog",
            {
                changelogFile: "CHANGELOG.md",
                changelogTitle: "Changelog"
            }
        ],
        [
            "@semantic-release/git",
            {
                assets: [
                    "CHANGELOG.md",
                ]
            }
        ],
        "@semantic-release/github",
        [
            "semantic-release-hackage",
            {
                "versionPrefix": "0.",
                "publishDocumentation": true
            }
        ]
    ]
}
