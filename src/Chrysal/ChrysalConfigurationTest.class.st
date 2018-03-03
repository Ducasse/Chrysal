Class {
	#name : #ChrysalConfigurationTest,
	#superclass : #TestCase,
	#category : #'Chrysal-Tests'
}

{ #category : #tests }
ChrysalConfigurationTest >> sample [
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
ChrysalConfigurationTest >> testImportFromDictionary [
	| conf |
	conf := ChrysalConfiguration
		newFromDictionary:
			{('array' -> #()).
			('title' -> 'supercool book').
			('authors' -> 'stef zegreat')} asDictionary.
	self assert: (conf propertyAt: 'title') equals: 'supercool book'.
	self assert: (conf propertyAt: 'array') equals: #().
	self assert: (conf propertyAt: 'authors') equals: 'stef zegreat'
]

{ #category : #tests }
ChrysalConfigurationTest >> testImportFromDictionaryWithConversion [
	| conf |
	conf := ConfigurationForXX newFromDictionary: {('verbose' -> 'true') . ( 'htmlChapterTemplate' -> '_support/templates/html.mustache')} asDictionary.
	self assert: (conf propertyAt: 'verbose') equals: true	" and not 'true' ".
	self assert: (conf propertyAt: 'htmlChapterTemplate') class equals: FileReference
]

{ #category : #tests }
ChrysalConfigurationTest >> testImportFromFile [
	| config fileSystem |
	fileSystem := FileSystem memory root.
	fileSystem / 'test.conf' writeStreamDo: [ :st | st nextPutAll: self sample ].
	config := ConfigurationForXX newFromFile: fileSystem / 'test.conf'.
	self assert: (config propertyAt: 'verbose')
]

{ #category : #tests }
ChrysalConfigurationTest >> testImportFromString [
	| ston conf |
	ston := '{
		"array": [  ],
		"title": "supercool book",
		"authors":"stef zegreat" }}'.
	conf := ChrysalConfiguration newFromString: ston.
	self assert: (conf propertyAt: 'title') equals: 'supercool book'.
	self assert: (conf propertyAt: 'array') equals: #().
	self assert: (conf propertyAt: 'authors') equals: 'stef zegreat'
]

{ #category : #tests }
ChrysalConfigurationTest >> testNewFromStream [
	| conf |
	conf := ConfigurationForXX newFromStream: self sample readStream.
	self assert: conf verbose
]

{ #category : #tests }
ChrysalConfigurationTest >> testSetConvertedValueOf [
	| conf |
	conf := ConfigurationForXX
		newFromStream: self sample readStream.
	conf setConvertedValueOf: 'outputDirectory' -> 'build'.
	self assert: conf outputDirectory class equals: FileReference.
	self assert: conf verbose class equals: True
]
