Class {
	#name : #ConfigurationForXX,
	#superclass : #ConfigurationRootForXX,
	#category : #'Chrysal-Tests'
}

{ #category : #description }
ConfigurationForXX class >> chrysalDescriptions [
 ^ STON fromString: '[
	StringConfigurationItem {
		#propertyName : #title,
		#default : ''my super cool book'',
		#noDefault : false
	},
	NumberConfigurationItem {
		#propertyName : #headingLevelOffset,
		#default : 0,
		#noDefault : false
	},
	BooleanConfigurationItem {
		#propertyName : #verbose,
		#default : ''true'',
		#noDefault : false
	},
	StringConfigurationItem {
		#propertyName : #attribution,
		#default : ''me, myself and I'',
		#noDefault : false
	},
	StringConfigurationItem {
		#propertyName : #series,
		#default : ''Square Bracket Associate Collection'',
		#noDefault : false
	},
	StringConfigurationItem {
		#propertyName : #keywords,
		#default : ''Pharo'',
		#noDefault : false
	},
	FolderConfigurationItem {
		#propertyName : #outputDirectory,
		#default : ''build'',
		#noDefault : false
	},
	FileConfigurationItem {
		#propertyName : #mainDocument,
		#default : ''book'',
		#noDefault : false
	},
	FileConfigurationItem {
		#propertyName : #latexTemplate,
		#default : ''_support/templates/main.latex.mustache'',
		#noDefault : false
	},
	FileConfigurationItem {
		#propertyName : #latexChapterTemplate,
		#default : ''_support/templates/chapter.latex.mustache'',
		#noDefault : false
	},
	FileConfigurationItem {
		#propertyName : #htmlTemplate,
		#default : ''_support/templates/html.mustache'',
		#noDefault : false
	},
	FileConfigurationItem {
		#propertyName : #htmlChapterTemplate,
		#default : ''_support/templates/html.mustache'',
		#noDefault : false
	},
	NewLineConfigurationItem {
		#propertyName : #newLine,
		#default : #unix,
		#explanation : ''The string that separates lines in the exported document. This is often either LF or CR\\+LF but any string is possible. Values are #(#cr #lf #crlf #unix #mac #dos)'',
		#noDefault : false,
		#defaultKey : #unix
	},
	SymbolConfigurationItem {
		#propertyName : #latexWriter,
		#default : #''latex:sbabook'',
		#noDefault : false
	},
	CompositeConfigurationItem {
		#propertyName : #printerComposite,
		#default : #ChrysalCompositeDomain,
		#noDefault : false,
		#fields : [
			#level,
			#renderAs,
			#capitalization,
			#numbering,
			#headerSize
		]
	},
	CompositeConfigurationItem {
		#propertyName : #convertedComposite,
		#default : #ChrysalConvertedCompositeDomain,
		#noDefault : false,
		#fields : [
			#newLine,
			#htmlTemplate,
			#title
		]
	},
	ListConfigurationItem {
		#propertyName : #levels,
		#default : #OrderedCollection,
		#noDefault : false,
		#element : ''printerComposite''
	},
	PathConfigurationItem {
		#propertyName : #relativeSimple,
		#default : ''simple'',
		#noDefault : false
	},
	PathConfigurationItem {
		#propertyName : #relativeComplex,
		#default : ''simple/simple1'',
		#noDefault : false
	},
	PathConfigurationItem {
		#propertyName : #absoluteSimple,
		#default : ''/simple'',
		#noDefault : false
	},
	PathConfigurationItem {
		#propertyName : #absoluteComplex,
		#default : ''/simple/simple1'',
		#noDefault : false
	},
	StringConfigurationItem {
		#propertyName : #titleWithoutDefault,
		#default : ''my super cool book'',
		#noDefault : true
	}
]'
]

{ #category : #accessing }
ConfigurationForXX >> absoluteComplex [
	"generated code"
	^ self propertyAt: #absoluteComplex ifAbsent: [Path from: '/simple/simple1']
]

{ #category : #accessing }
ConfigurationForXX >> absoluteComplex: aValue [
	"generated code"
	^ self propertyAt: #absoluteComplex put: aValue
]

{ #category : #testing }
ConfigurationForXX >> absoluteComplexIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self absoluteComplex
]

{ #category : #accessing }
ConfigurationForXX >> absoluteSimple [
	"generated code"
	^ self propertyAt: #absoluteSimple ifAbsent: [Path from: '/simple']
]

{ #category : #accessing }
ConfigurationForXX >> absoluteSimple: aValue [
	"generated code"
	^ self propertyAt: #absoluteSimple put: aValue
]

{ #category : #testing }
ConfigurationForXX >> absoluteSimpleIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self absoluteSimple
]

{ #category : #accessing }
ConfigurationForXX >> attribution [
	"generated code"
	^ self propertyAt: #attribution ifAbsent: ['me, myself and I']
]

{ #category : #accessing }
ConfigurationForXX >> attribution: aValue [
	"generated code"
	^ self propertyAt: #attribution put: aValue
]

{ #category : #testing }
ConfigurationForXX >> attributionIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self attribution
]

{ #category : #conversion }
ConfigurationForXX >> convertAbsoluteComplex: aValue [
	"generated code"
	^ Path from: aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertAbsoluteSimple: aValue [
	"generated code"
	^ Path from: aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertAttribution: aValue [
	"generated code"
	^ aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertConvertedComposite: aValue [
	"generated code"
	| composite |
	composite := ChrysalConvertedCompositeDomain new.
	aValue do: [:v | 
		    "We ignore undeclared fields"
			(#(#newLine #htmlTemplate #title) includes: v key asSymbol)
				ifTrue: [ 
					| value |
					"if the element has been described we convert"
					(self declaredItems includes: v key asSymbol)
						ifTrue: [ value := self perform: (#convert, v key capitalized, ':') asSymbol with: v value ]
						ifFalse: [ value := v value ].		
					composite 
								perform:  (v key, ':') asSymbol 
								with: value].
		].
	^ composite
]

{ #category : #conversion }
ConfigurationForXX >> convertHeadingLevelOffset: aValue [
	"generated code"
	^ aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertHtmlChapterTemplate: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForXX >> convertHtmlTemplate: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForXX >> convertKeywords: aValue [
	"generated code"
	^ aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertLatexChapterTemplate: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForXX >> convertLatexTemplate: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForXX >> convertLatexWriter: aValue [
	"generated code"
	^ aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertLevels: aValue [
	"generated code"
	| elements |
	elements := OrderedCollection new.
	aValue do: [:v | 
		 elements add: (self convertPrinterComposite: v ) ].
	^ elements
]

{ #category : #conversion }
ConfigurationForXX >> convertMainDocument: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForXX >> convertNewLine: aValue [
	"generated code"
	^ (Dictionary
			newFrom:
				{(#cr -> String cr).
				(#lf -> UnixPlatform new lineEnding).
				(#crlf -> Win32Platform new lineEnding).
				(#unix -> UnixPlatform new lineEnding).
				(#mac -> UnixPlatform new lineEnding).
				(#dos -> Win32Platform new lineEnding)}) at: aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertOutputDirectory: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForXX >> convertPrinterComposite: aValue [
	"generated code"
	| composite |
	composite := ChrysalCompositeDomain new.
	aValue do: [:v | 
		    "We ignore undeclared fields"
			(#(#level #renderAs #capitalization #numbering #headerSize) includes: v key asSymbol)
				ifTrue: [ 
					| value |
					"if the element has been described we convert"
					(self declaredItems includes: v key asSymbol)
						ifTrue: [ value := self perform: (#convert, v key capitalized, ':') asSymbol with: v value ]
						ifFalse: [ value := v value ].		
					composite 
								perform:  (v key, ':') asSymbol 
								with: value].
		].
	^ composite
]

{ #category : #conversion }
ConfigurationForXX >> convertRelativeComplex: aValue [
	"generated code"
	^ Path from: aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertRelativeSimple: aValue [
	"generated code"
	^ Path from: aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertSeries: aValue [
	"generated code"
	^ aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertTitle: aValue [
	"generated code"
	^ aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertTitleWithoutDefault: aValue [
	"generated code"
	^ aValue
]

{ #category : #conversion }
ConfigurationForXX >> convertVerbose: aValue [
	"generated code"
	^ aValue 
]

{ #category : #accessing }
ConfigurationForXX >> convertedComposite [
	"generated code"
	^ self propertyAt: #convertedComposite ifAbsent: [ ChrysalConvertedCompositeDomain new ]
]

{ #category : #accessing }
ConfigurationForXX >> convertedComposite: aValue [
	"generated code"
	^ self propertyAt: #convertedComposite put: aValue
]

{ #category : #testing }
ConfigurationForXX >> convertedCompositeIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self convertedComposite
]

{ #category : #'declared items' }
ConfigurationForXX >> declaredItems [
	"generated code"
	^ #( #title #headingLevelOffset #verbose #attribution #series #keywords #outputDirectory #mainDocument #latexTemplate #latexChapterTemplate #htmlTemplate #htmlChapterTemplate #newLine #latexWriter #printerComposite #convertedComposite #levels #relativeSimple #relativeComplex #absoluteSimple #absoluteComplex #titleWithoutDefault )
]

{ #category : #accessing }
ConfigurationForXX >> headingLevelOffset [
	"generated code"
	^ self propertyAt: #headingLevelOffset ifAbsent: [0]
]

{ #category : #accessing }
ConfigurationForXX >> headingLevelOffset: aValue [
	"generated code"
	^ self propertyAt: #headingLevelOffset put: aValue
]

{ #category : #testing }
ConfigurationForXX >> headingLevelOffsetIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self headingLevelOffset
]

{ #category : #accessing }
ConfigurationForXX >> htmlChapterTemplate [
	"generated code"
	^ self propertyAt: #htmlChapterTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/html.mustache')]
]

{ #category : #accessing }
ConfigurationForXX >> htmlChapterTemplate: aValue [
	"generated code"
	^ self propertyAt: #htmlChapterTemplate put: aValue
]

{ #category : #testing }
ConfigurationForXX >> htmlChapterTemplateIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self htmlChapterTemplate
]

{ #category : #accessing }
ConfigurationForXX >> htmlTemplate [
	"generated code"
	^ self propertyAt: #htmlTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/html.mustache')]
]

{ #category : #accessing }
ConfigurationForXX >> htmlTemplate: aValue [
	"generated code"
	^ self propertyAt: #htmlTemplate put: aValue
]

{ #category : #testing }
ConfigurationForXX >> htmlTemplateIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self htmlTemplate
]

{ #category : #accessing }
ConfigurationForXX >> keywords [
	"generated code"
	^ self propertyAt: #keywords ifAbsent: ['Pharo']
]

{ #category : #accessing }
ConfigurationForXX >> keywords: aValue [
	"generated code"
	^ self propertyAt: #keywords put: aValue
]

{ #category : #testing }
ConfigurationForXX >> keywordsIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self keywords
]

{ #category : #accessing }
ConfigurationForXX >> latexChapterTemplate [
	"generated code"
	^ self propertyAt: #latexChapterTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/chapter.latex.mustache')]
]

{ #category : #accessing }
ConfigurationForXX >> latexChapterTemplate: aValue [
	"generated code"
	^ self propertyAt: #latexChapterTemplate put: aValue
]

{ #category : #testing }
ConfigurationForXX >> latexChapterTemplateIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self latexChapterTemplate
]

{ #category : #accessing }
ConfigurationForXX >> latexTemplate [
	"generated code"
	^ self propertyAt: #latexTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/main.latex.mustache')]
]

{ #category : #accessing }
ConfigurationForXX >> latexTemplate: aValue [
	"generated code"
	^ self propertyAt: #latexTemplate put: aValue
]

{ #category : #testing }
ConfigurationForXX >> latexTemplateIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self latexTemplate
]

{ #category : #accessing }
ConfigurationForXX >> latexWriter [
	"generated code"
	^ self propertyAt: #latexWriter ifAbsent: [#latex:sbabook]
]

{ #category : #accessing }
ConfigurationForXX >> latexWriter: aValue [
	"generated code"
	^ self propertyAt: #latexWriter put: aValue
]

{ #category : #testing }
ConfigurationForXX >> latexWriterIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self latexWriter
]

{ #category : #accessing }
ConfigurationForXX >> levels [
	"generated code"
	^ self propertyAt: #levels ifAbsent: [ OrderedCollection new ]
]

{ #category : #accessing }
ConfigurationForXX >> levels: aValue [
	"generated code"
	^ self propertyAt: #levels put: aValue
]

{ #category : #testing }
ConfigurationForXX >> levelsIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self levels
]

{ #category : #accessing }
ConfigurationForXX >> mainDocument [
	"generated code"
	^ self propertyAt: #mainDocument ifAbsent: [(FileSystem workingDirectory / 'book')]
]

{ #category : #accessing }
ConfigurationForXX >> mainDocument: aValue [
	"generated code"
	^ self propertyAt: #mainDocument put: aValue
]

{ #category : #testing }
ConfigurationForXX >> mainDocumentIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self mainDocument
]

{ #category : #accessing }
ConfigurationForXX >> newLine [
	"generated code"
	^ self propertyAt: #newLine ifAbsent: [UnixPlatform new lineEnding]
]

{ #category : #accessing }
ConfigurationForXX >> newLine: aValue [
	"generated code"
	^ self propertyAt: #newLine put: aValue
]

{ #category : #testing }
ConfigurationForXX >> newLineIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self newLine
]

{ #category : #accessing }
ConfigurationForXX >> outputDirectory [
	"generated code"
	^ self propertyAt: #outputDirectory ifAbsent: [(FileSystem workingDirectory / 'build')]
]

{ #category : #accessing }
ConfigurationForXX >> outputDirectory: aValue [
	"generated code"
	^ self propertyAt: #outputDirectory put: aValue
]

{ #category : #testing }
ConfigurationForXX >> outputDirectoryIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self outputDirectory
]

{ #category : #accessing }
ConfigurationForXX >> printerComposite [
	"generated code"
	^ self propertyAt: #printerComposite ifAbsent: [ ChrysalCompositeDomain new ]
]

{ #category : #accessing }
ConfigurationForXX >> printerComposite: aValue [
	"generated code"
	^ self propertyAt: #printerComposite put: aValue
]

{ #category : #testing }
ConfigurationForXX >> printerCompositeIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self printerComposite
]

{ #category : #accessing }
ConfigurationForXX >> relativeComplex [
	"generated code"
	^ self propertyAt: #relativeComplex ifAbsent: [Path from: 'simple/simple1']
]

{ #category : #accessing }
ConfigurationForXX >> relativeComplex: aValue [
	"generated code"
	^ self propertyAt: #relativeComplex put: aValue
]

{ #category : #testing }
ConfigurationForXX >> relativeComplexIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self relativeComplex
]

{ #category : #accessing }
ConfigurationForXX >> relativeSimple [
	"generated code"
	^ self propertyAt: #relativeSimple ifAbsent: [Path from: 'simple']
]

{ #category : #accessing }
ConfigurationForXX >> relativeSimple: aValue [
	"generated code"
	^ self propertyAt: #relativeSimple put: aValue
]

{ #category : #testing }
ConfigurationForXX >> relativeSimpleIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self relativeSimple
]

{ #category : #accessing }
ConfigurationForXX >> series [
	"generated code"
	^ self propertyAt: #series ifAbsent: ['Square Bracket Associate Collection']
]

{ #category : #accessing }
ConfigurationForXX >> series: aValue [
	"generated code"
	^ self propertyAt: #series put: aValue
]

{ #category : #testing }
ConfigurationForXX >> seriesIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self series
]

{ #category : #accessing }
ConfigurationForXX >> title [
	"generated code"
	^ self propertyAt: #title ifAbsent: ['my super cool book']
]

{ #category : #accessing }
ConfigurationForXX >> title: aValue [
	"generated code"
	^ self propertyAt: #title put: aValue
]

{ #category : #testing }
ConfigurationForXX >> titleIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self title
]

{ #category : #accessing }
ConfigurationForXX >> titleWithoutDefault [
	"generated code"
	^ self propertyAt: #titleWithoutDefault ifAbsent: ['my super cool book']
]

{ #category : #accessing }
ConfigurationForXX >> titleWithoutDefault: aValue [
	"generated code"
	^ self propertyAt: #titleWithoutDefault put: aValue
]

{ #category : #testing }
ConfigurationForXX >> titleWithoutDefaultIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ noDefBlock value
]

{ #category : #accessing }
ConfigurationForXX >> verbose [
	"generated code"
	^ self propertyAt: #verbose ifAbsent: [true]
]

{ #category : #accessing }
ConfigurationForXX >> verbose: aValue [
	"generated code"
	^ self propertyAt: #verbose put: aValue
]

{ #category : #testing }
ConfigurationForXX >> verboseIfDefined: defBlockWithOneArg undefined: noDefBlock [
	"generated code""When the property has been declared as having a default, we pass its value defined or default to the argument, defBlockWithOneArg.
Else we execute simply the noDefBlock. Since this code is statically generated we know which branch to take at compilation time."
	 ^ defBlockWithOneArg value: self verbose
]
