---
description: How to release a new version of the package
---
# Release Package Version

This workflow describes the steps to release a new version of the `OmopHelpers` package.

1.  **Update NEWS.md**: Add a new entry for the version being released, documenting the changes.
2.  **Bump Version**: Update the `Version` field in the `DESCRIPTION` file.
3.  **Commit Changes**: Commit the `NEWS.md` and `DESCRIPTION` updates with a message like "Bump version to X.Y.Z".
4.  **Create Tag**: Create a git tag for the new version (e.g., `vX.Y.Z`).
5.  **Push**: Push the commit and the tag to the remote repository.

Example commands:
```bash
# After editing files
git add DESCRIPTION NEWS.md
git commit -m "Bump version to 0.1.1"
git tag v0.1.1
git push && git push --tags
```
