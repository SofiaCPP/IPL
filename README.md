# Implementation of Programming Languages

This is the code of the website of a course on implementing a programming
language. The website is hosted at https://sofiacpp.github.io/IPL.

## Working with the website

The website is deployed as GitHub pages from the contents of the gh-pages branch
of this repository.


### Requirements

- Git and Git-Bash on Windows
- [hugo](https://gohugo.io)

### Checking out the code

Simply clone this repository.

### Setting up the clone

1. Make sure that `git` and `hugo` are in the path
2. Add the gh-pages as a *git worktree* under the public folder

        git worktree add public gh-pages

### Creating new content

#### Creating new posts

The normal hugo way of creating new posts applies

    hugo new content/posts/post.md
    hugo new content/page/page.md

Creating new slides requires using a different template. You can use the
*content/slides* as boilerplate.

    cp -R content/slides content/myslides
    edit content/myslides/_index.md

### Publishing changes

1. Make any changes to the content and **COMMIT**
2. Run the publish.sh script

        sh publish.sh

    It will generate the new version inside the *public* folder, commit and push
    it.
