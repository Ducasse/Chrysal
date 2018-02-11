Class {
	#name : #ConfigurationTest,
	#superclass : #TestCase,
	#category : #'Chrysal-Tests'
}

{ #category : #tests }
ConfigurationTest >> sample [
	^ '{
  "title": "A Little Scheme in Pharo",
  "verbose":"true",
  "attribution": "Stéphane Ducasse with Guillermo Polito",
  "series": "Square Bracket tutorials",
  "keywords": "project template, Pillar, Pharo, Smalltalk",
  "outputDirectory": "build",
  "mainDocument": "book",
  "latexTemplate": "_support/templates/main.latex.mustache",
  "latexChapterTemplate": "_support/templates/chapter.latex.mustache",
  "htmlTemplate": "_support/templates/html.mustache",
  "htmlChapterTemplate": "_support/templates/html.mustache",
  "newLine": #unix,
  "latexWriter": #''latex:sbabook''
}'
]

{ #category : #tests }
ConfigurationTest >> testNewFromStream [

	ConfigurationForXX newFromStream: self sample readStream
]

{ #category : #tests }
ConfigurationTest >> testSetConvertedValueOf [

	| conf |
	conf := ConfigurationForXX new. 
	conf setConvertedValueOf: ('outputDirectory' -> 'build').
	self assert: conf outputDirectory class equals: FileReference
]
