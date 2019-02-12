# LaCasa

LaCasa is a type system and programming model to enforce the object
capability discipline in Scala, and to provide unique object
references. One important application of LaCasa's type system is
software isolation of concurrent processes. Isolation is important for
several reasons including security and data-race freedom. Moreover,
LaCasa's unique references enable efficient, by-reference message
passing while guaranteeing a "deep-copy" semantics. This deep-copy
semantics enables programmers to seamlessly port concurrent programs
running on a single machine to distributed programs running on
large-scale clusters of machines.

Paper (preprint): Philipp Haller and Alex Loiko.
                  [LaCasa: Lightweight Affinity and Object Capabilities in Scala](http://www.csc.kth.se/~phaller/doc/haller16-oopsla.pdf).
                  Proc. OOPSLA. ACM, 2016. [[ACM DL](http://dl.acm.org/citation.cfm?id=2984042)] [[DOI](https://doi.org/10.1145/2983990.2984042)]

Talk at Scala World '16: [video](https://www.youtube.com/watch?v=nwWvPeX6U9w),
[slides](https://speakerdeck.com/phaller/can-we-make-concurrency-in-scala-safer)

Talk at OOPSLA '16: [slides](https://speakerdeck.com/phaller/lacasa-lightweight-affinity-and-object-capabilities-in-scala-1)

## Building

Building LaCasa requires [sbt](http://www.scala-sbt.org/). The build
consists of two steps. The first step consists of building and
packaging the LaCasa compiler plugin:
```
sbt 'project lacasa-plugin' package
```

The second step consists of building and running projects like
`lacasa-samples` which depend on the packaged plugin. This requires
starting sbt with the `lacasa.plugin.jar` JVM system property set to
the path of the packaged plugin:
```
sbt -Dlacasa.plugin.jar=plugin/target/scala-2.11/lacasa-plugin_2.11.7-0.1.0-SNAPSHOT.jar
```

Build and run samples:
```
> project lacasa-samples
> run
```

## Testing

Once sbt has been started with the `lacasa.plugin.jar` JVM system
property set, the plugin tests can be run as follows:
```
> project lacasa-plugin
> test
```

The plugin tests are located in directory
`plugin/src/test/scala/lacasa/`.
