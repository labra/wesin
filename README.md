wesin
=====
[![Build Status](https://travis-ci.org/labra/wesin.svg?branch=master)](https://travis-ci.org/labra/wesin)
[![codecov.io](http://codecov.io/github/labra/wesin/coverage.svg?branch=master)](http://codecov.io/github/labra/wesin?branch=master)


Web Semantics Library in Scala

This repository contains several tools to work with RDF in Scala

* A Turtle Parser that conforms to W3c RDF 1.1 tests based on Scala Parser Combinators
* An inductive representation of RDF graphs
* A simple Jena to Scala mapper
* This library is used by [ShExcala](http://labra.github.io/shexcala), an RDF validator using Shape Expressions 

Links
=====

* [More info](http://labra.github.io/wesin)
* [mailing list](https://groups.google.com/forum/?hl=en&fromgroups#!forum/wesin) 
* [Binaries](https://bintray.com/weso/weso-releases/wesin/view)

Author
======

[Jose Emilio Labra Gayo](http://www.di.uniovi.es/~labra), [WESO Research Group](http://www.weso.es)

Related Projects
================

[Banana-RDF](https://github.com/w3c/banana-rdf) contains a more general library to work with RDF from Scala. 
It allows different underlying implementations like Jena and Sesame. 
A long term project will be to unify our work with that library.  
