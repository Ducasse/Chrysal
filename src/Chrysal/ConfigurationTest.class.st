Class {
	#name : #ConfigurationTest,
	#superclass : #TestCase,
	#category : #Chrysal
}

{ #category : #tests }
ConfigurationTest >> sample [

	^ '{
  "title": "A Little Scheme in Pharo",
  "attribution": "Stéphane Ducasse with Guillermo Polito",
  "series": "Square Bracket tutorials",
  "keywords": "project template, Pillar, Pharo, Smalltalk",
  "outputDirectory": "build",
  "mainDocument": "book",
  "latexTemplate": "_support/templates/main.latex.mustache",
  "latexChapterTemplate": "_support/templates/chapter.latex.mustache",
  "htmlTemplate": "_support/templates/html.mustache",
  "htmlChapterTemplate": "_support/templates/html.mustache",
  "chapters": [ 
	"Chapters/Scheme"
	],
  "newLine": #unix,
  "latexWriter": #''latex:sbabook''
}'
]

{ #category : #tests }
ConfigurationTest >> testNewFromStream [

	Configuration newFromStream: self sample.
]
