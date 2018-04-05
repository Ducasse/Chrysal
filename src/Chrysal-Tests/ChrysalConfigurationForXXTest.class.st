"
Check ChrysalConfigurationBuilder >> #itemDescriptionForXX for the definition of the description items. 


ChrysalConfigurationBuilder new 
	configurationRootClass: ConfigurationRootForXX;
	defineConfigurationClassNamed: #ConfigurationForXX 
	packagedIn: 'Chrysal-Tests'; 
	withDescriptionItems: ChrysalConfigurationBuilder itemDescriptionForXX
"
Class {
	#name : #ChrysalConfigurationForXXTest,
	#superclass : #TestCase,
	#category : #'Chrysal-Tests'
}

{ #category : #hooks }
ChrysalConfigurationForXXTest >> configurationClass [
	^ ConfigurationBuilderTest ensureConfigurationForXXExist
]

{ #category : #resources }
ChrysalConfigurationForXXTest >> listSample [
	^  '{
  "title": "A Little Scheme in Pharo",
  ''printer'' : [
		[    ''level'' : 1,
			''renderAs'' : ''letter'',
			''capitalization'' : ''uppercase'',
			''numbering'' : true,
			''size'' : 2
		],
		[	''level'' : 2,
			''renderAs'' : ''number'',
			''capitalization'' : ''uppercase'',
			''numbering'' : true,
			''size'' : 3
		]
	]
}'
]

{ #category : #resources }
ChrysalConfigurationForXXTest >> sample [
	^  '{
  "title": "A Little Scheme in Pharo",
  "verbose":true,
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

{ #category : #'tests - composite' }
ChrysalConfigurationForXXTest >> testComposite [
	|  config composite |
	config := self configurationClass newFromDictionary:  (STON fromString: '{
	''printerComposite'' : 
		[ 
		''level'' : 1,
		''renderAs'' : ''letter'',
		''capitalization'' : ''uppercase'',
		''numbering'' : true,
		''headerSize'' : 2
		]
	}').
	composite := config propertyAt: 'printerComposite'. 
	self assert: composite capitalization equals: 'uppercase'.
	self assert: composite level equals: 1.
	self assert: composite numbering.
	self assert: composite headerSize equals: 2.
	
	
]

{ #category : #'tests - composite' }
ChrysalConfigurationForXXTest >> testCompositeDiscardUndeclaredField [
	|  config composite |
	config := self configurationClass newFromDictionary:  (STON fromString: '{
	''printerComposite'' : 
		[ 
		''level'' : 1,
		''renderAs'' : ''letter'',
		''capitalization'' : ''uppercase'',
		''numbering'' : true,
		''headerSize'' : 2,
		"undeclaredField": true
		]
	}').
	composite := config propertyAt: 'printerComposite'. 
	self assert: composite capitalization equals: 'uppercase'.
	self assert: composite level equals: 1.
	self assert: composite numbering.
	self assert: composite headerSize equals: 2.
	
	
]

{ #category : #'tests - composite' }
ChrysalConfigurationForXXTest >> testConvertingComposite [
	|  config composite |
	config := self configurationClass  newFromDictionary:  (STON fromString: '{
		"convertedComposite": [
			"title": "supercool book",
			"htmlTemplate": "_support/templates/html.mustache",
			"newLine":#unix,
			"undeclared":"true" ]
		}').
	composite :=config propertyAt: 'convertedComposite'. 
	self assert: composite title equals: 'supercool book'.
	self assert: composite htmlTemplate class equals: FileReference.
	self assert: composite newLine equals: UnixPlatform new lineEnding
	
	
]

{ #category : #'tests - creation from ston' }
ChrysalConfigurationForXXTest >> testImportFromDictionaryWithConversion [
	| conf |
	conf := ConfigurationForXX newFromSTONDictionary: {('verbose' -> true) . ( 'htmlChapterTemplate' -> '_support/templates/html.mustache')} asDictionary.
	self assert: (conf propertyAt: 'verbose') equals: true	" and not 'true' ".
	self assert: (conf propertyAt: 'htmlChapterTemplate') class equals: FileReference
]

{ #category : #'tests - creation from ston' }
ChrysalConfigurationForXXTest >> testImportFromFile [
	| config fileSystem |
	fileSystem := FileSystem memory root.
	fileSystem / 'test.conf'
		writeStreamDo: [ :st | st nextPutAll: self sample ].
	config := self configurationClass
		newFromFile: fileSystem / 'test.conf'.
	self assert: (config propertyAt: 'verbose')
]

{ #category : #'tests - creation from pharo' }
ChrysalConfigurationForXXTest >> testImportFromPharoDictionary [
	| conf |
	conf := ConfigurationForXX
		newFromConvertedDictionary:
			{('array' -> #()).
			('verbose' -> true).
			('title' -> 'supercool book').
			('authors' -> 'stef zegreat')} asDictionary.
	self assert: (conf propertyAt: 'title') equals: 'supercool book'.
	self assert: (conf propertyAt: 'array') equals: #().
	self assert: (conf propertyAt: 'authors') equals: 'stef zegreat'.
	self assert: conf verbose.
	
	conf :=  ConfigurationForXX
		newFromConvertedDictionary:
			{('array' -> #()).
			('verbose' -> 'true' ) } asDictionary.
	self assert: conf verbose equals: 'true'
	
]

{ #category : #'tests - creation from ston' }
ChrysalConfigurationForXXTest >> testImportFromSTONDictionary [
	
	| conf |
	conf := ConfigurationForXX
		newFromSTONDictionary:
			{('verbose' -> true).
			('title' -> 'supercool book').
			('authors' -> 'stef zegreat')} asDictionary.
	self assert: (conf propertyAt: 'title') equals: 'supercool book'.
	self assert: (conf propertyAt: 'authors') equals: 'stef zegreat'.
	self assert: conf verbose
]

{ #category : #'tests - creation from ston' }
ChrysalConfigurationForXXTest >> testImportFromSTONDictionaryWithExtended [
	
	| conf |
	conf := ConfigurationForXX 
				newFromDictionary:  {('verbose' -> true). ('title' -> 'supercool book') } asDictionary
				extendedWith: {('authors' -> 'stef zegreat')} asDictionary.
	self assert: (conf propertyAt: 'title') equals: 'supercool book'.
	self assert: (conf propertyAt: 'authors') equals: 'stef zegreat'.
	self assert: conf verbose
]

{ #category : #'tests - value types' }
ChrysalConfigurationForXXTest >> testImportFromString [
	| ston conf |
	ston := '{
	      "verbose": true,
		"undeclaredBoolean":"true",
		"title": "supercool book",
		"headingLevelOffset":3,
		"outputDirectory": "build",
		"htmlChapterTemplate": "_support/templates/html.mustache",
		"newLine":#unix
		}}'.
	conf := self configurationClass newFromString: ston.
	self assert: conf verbose.
	self assert: (conf propertyAt: 'undeclaredBoolean') equals: 'true'.
	self assert: conf headingLevelOffset equals: 3.
	self assert: conf title equals: 'supercool book'.
	self assert: conf outputDirectory class equals: FileReference.
	self assert: conf htmlChapterTemplate class equals: FileReference.
	self assert: conf newLine equals: UnixPlatform new lineEnding
]

{ #category : #'tests - composite' }
ChrysalConfigurationForXXTest >> testList [
	|  config list |
	config := self configurationClass newFromDictionary:  (STON fromString: '  { ''levels'' : [
		[    ''level'' : 1,
			''renderAs'' : ''letter'',
			''capitalization'' : ''uppercase'',
			''numbering'' : true,
			''headerSize'' : 2
		],
		[	''level'' : 2,
			''renderAs'' : ''number'',
			''capitalization'' : ''uppercase'',
			''numbering'' : true,
			''headerSize'' : 3
		]
	]}').
	list := config propertyAt: 'levels'. 
	self assert: list size equals: 2.
	self assert: list first class equals: ChrysalCompositeDomain.
	self assert: list first capitalization equals: 'uppercase'.
	self assert: list first level equals: 1.
	self assert: list first numbering.
	self assert: list first headerSize equals: 2.
	
	
]

{ #category : #'tests - creation from ston' }
ChrysalConfigurationForXXTest >> testNewFromStream [
	| conf |
	conf := self configurationClass newFromStream: self sample readStream.
	self assert: conf verbose
]

{ #category : #'tests - composite' }
ChrysalConfigurationForXXTest >> testPostTreat [
	|  config list |
	config := self configurationClass newFromDictionary:  (STON fromString: '  { ''levels'' : [
		[    ''level'' : 1,
			''renderAs'' : ''letter'',
			''capitalization'' : ''uppercase'',
			''numbering'' : true,
			''headerSize'' : 2
		],
		[	''level'' : 2,
			''renderAs'' : ''number'',
			''capitalization'' : ''uppercase'',
			''numbering'' : true,
			''headerSize'' : 3
		]
	]}').
	list := config propertyAt: 'levels'. 
	self assert: (config  hasProperty: #newLevels)
]

{ #category : #'tests - composite' }
ChrysalConfigurationForXXTest >> testSetConvertedValueOf [
	| conf |
	conf := self configurationClass newFromStream: self sample readStream.
	conf setConvertedValueOf: 'outputDirectory' -> 'build'.
	self assert: conf outputDirectory class equals: FileReference.
	self assert: conf verbose class equals: True
]
