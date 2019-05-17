## FPA Introduction Course

Just clone the project and follow the comments in the code while having `sbt` run tests continuously.

```
fpa-course-intro ∴ sbt
[info] Loading project definition from /Users/rh83yy/code/fpa-course-intro/project
[info] Loading settings from build.sbt ...
[info] Set current project to fpa-course-intro (in build file:/Users/rh83yy/code/fpa-course-intro/)
[info] sbt server started at local:///Users/rh83yy/.sbt/1.0/server/7b53614b6829301d17b1/sock
sbt:fpa-course-intro> ~test
[info] HelloSpec:
[info] The Hello object
[info] - should say hello
[info] Run completed in 401 milliseconds.
[info] Total number of tests run: 1
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 1, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[success] Total time: 1 s, completed Jul 26, 2018 10:01:58 AM
1. Waiting for source changes... (press enter to interrupt)
```

The tests associated which each individual chapter are configured to be ignored.  Remove the `@Ignore` annotation for the chapter you want to work on.  Also, lots of the course is WIP, so expect parts to be commented out sometimes.  Just play around and have fun.

---

## History

- 2018-07-19 : Kick Off
- 2018-07-26 : Type Classes
- 2018-08-02 : Map and FlatMap
- 2018-08-09 : Properties
- 2018-08-16 : Validation
- 2018-08-23 : Cats Run Through
- 2018-08-30 : IO Monad
- 2018-09-04 : Recursion - Monad Transformers
- 2018-09-13 : List Comprehensions
- 2018-09-20 : Q&A
- 2018-09-28 : Monads versus the λ-calculus
- 2018-10-04 : Canceled
- 2018-10-11 : Y, oh Y, oh Y, oh Y?
- 2019-04-31 : Done
