Class {
	#name : #ConfigurationForPillar,
	#superclass : #ChrysalConfiguration,
	#category : #'Chrysal-ToBeMovedElsewhere'
}

{ #category : #description }
ConfigurationForPillar class >> chrysalDescriptions [
 ^ STON fromString: '[
	StringConfigurationItem {
		#propertyName : ''title'',
		#default : ''my super cool book''
	},
	StringConfigurationItem {
		#propertyName : ''attribution'',
		#default : ''me, myself and I''
	},
	NumberConfigurationItem {
		#propertyName : ''headingLevelOffset'',
		#default : 0,
		#explanation : ''Indicate how to convert from the level of a Pillar heading to the level of heading in your exported document.\r\tFor example, a headingLevelOffset of 3 converts a 1st level Pillar heading to an <h4>in HTML''
	},
	StringConfigurationItem {
		#propertyName : ''series'',
		#default : ''Square Bracket Associate Collection''
	},
	StringConfigurationItem {
		#propertyName : ''keywords'',
		#default : ''Pharo''
	},
	FolderConfigurationItem {
		#propertyName : ''outputDirectory'',
		#default : ''build''
	},
	FileConfigurationItem {
		#propertyName : ''outputFile'',
		#default : ''output'',
		#explanation : ''If separateOutputFiles is false, indicate the name of the output file. This can also be a write stream.''
	},
	FileConfigurationItem {
		#propertyName : ''mainDocument'',
		#default : ''book''
	},
	FileConfigurationItem {
		#propertyName : ''inputFile'',
		#default : ''book'',
		#explanation : ''The Pillar file that must be transformed. You can also specify an input file at the end of the command-line interface. Previously there was no default value''
	},
	FileConfigurationItem {
		#propertyName : ''latexTemplate'',
		#default : ''_support/templates/main.latex.mustache''
	},
	BooleanConfigurationItem {
		#propertyName : ''scrambledEmailAddresses'',
		#default : ''false''
	},
	BooleanConfigurationItem {
		#propertyName : ''separateOutputFiles'',
		#default : ''false'',
		#explanation : ''If true, each input file is exported to one output file. If false (the default), all input files are exported to outputFile.''
	},
	BooleanConfigurationItem {
		#propertyName : ''renderStructureAsSlide'',
		#default : ''false'',
		#explanation : ''When true (the default), Pillar will create a dedicated slide for each Pillar header. This parameter is meaningless when generating a written document''
	},
	FileConfigurationItem {
		#propertyName : ''latexChapterTemplate'',
		#default : ''_support/templates/chapter.latex.mustache''
	},
	FileConfigurationItem {
		#propertyName : ''htmlTemplate'',
		#default : ''_support/templates/html.mustache''
	},
	FileConfigurationItem {
		#propertyName : ''htmlChapterTemplate'',
		#default : ''_support/templates/html.mustache''
	},
	NewLineConfigurationItem {
		#propertyName : ''newLine'',
		#default : #unix,
		#explanation : ''The string that separates lines in the exported document. This is often either LF or CR\\+LF but any string is possible. Values are #(#cr #lf #crlf #unix #mac #dos)'',
		#defaultKey : #unix
	},
	StringConfigurationItem {
		#propertyName : ''latexWriter'',
		#default : #''latex:sbabook''
	},
	CodeBlockDefaultLanguageItem {
		#propertyName : #defaultScriptLanguage,
		#default : ''smalltalk'',
		#explanation : ''Indicate the language in scripts when none is specified. This language is used for syntax highlighting. The currently supported languages are bash, css, html, http, json, javascript, pillar, sql, ston, shellcommands and smalltalk''
	}
]'
]

{ #category : #accessing }
ConfigurationForPillar >> attribution [
	"generated code"
	^ self propertyAt: #attribution ifAbsent: ['me, myself and I']
]

{ #category : #accessing }
ConfigurationForPillar >> attribution: aValue [
	"generated code"
	^ self propertyAt: #attribution put: aValue
]

{ #category : #conversion }
ConfigurationForPillar >> convertAttribution: aValue [
	"generated code"
	^ aValue
]

{ #category : #conversion }
ConfigurationForPillar >> convertDefaultScriptLanguage: aValue [
	"generated code"
	^ PRAbstractScriptLanguage for: aValue
]

{ #category : #conversion }
ConfigurationForPillar >> convertHeadingLevelOffset: aValue [
	"generated code"
	^  aValue
]

{ #category : #conversion }
ConfigurationForPillar >> convertHtmlChapterTemplate: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForPillar >> convertHtmlTemplate: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForPillar >> convertInputFile: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForPillar >> convertKeywords: aValue [
	"generated code"
	^ aValue
]

{ #category : #conversion }
ConfigurationForPillar >> convertLatexChapterTemplate: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForPillar >> convertLatexTemplate: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForPillar >> convertLatexWriter: aValue [
	"generated code"
	^ aValue
]

{ #category : #conversion }
ConfigurationForPillar >> convertMainDocument: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForPillar >> convertNewLine: aValue [
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
ConfigurationForPillar >> convertOutputDirectory: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForPillar >> convertOutputFile: aValue [
	"generated code"
	^ (FileSystem workingDirectory / aValue)
]

{ #category : #conversion }
ConfigurationForPillar >> convertRenderStructureAsSlide: aValue [
	"generated code"
	^  aValue 
]

{ #category : #conversion }
ConfigurationForPillar >> convertScrambledEmailAddresses: aValue [
	"generated code"
	^  aValue 
]

{ #category : #conversion }
ConfigurationForPillar >> convertSeparateOutputFiles: aValue [
	"generated code"
	^  aValue 
]

{ #category : #conversion }
ConfigurationForPillar >> convertSeries: aValue [
	"generated code"
	^ aValue
]

{ #category : #conversion }
ConfigurationForPillar >> convertTitle: aValue [
	"generated code"
	^ aValue
]

{ #category : #'declared items' }
ConfigurationForPillar >> declaredItems [
	"generated code"
	^ #( #title #attribution #headingLevelOffset #series #keywords #outputDirectory #outputFile #mainDocument #inputFile #latexTemplate #scrambledEmailAddresses #separateOutputFiles #renderStructureAsSlide #latexChapterTemplate #htmlTemplate #htmlChapterTemplate #newLine #latexWriter #defaultScriptLanguage )
]

{ #category : #accessing }
ConfigurationForPillar >> defaultScriptLanguage [
	"generated code"
	^ self propertyAt: #defaultScriptLanguage ifAbsent: [(PRRealScriptLanguage for: 'smalltalk')]
]

{ #category : #accessing }
ConfigurationForPillar >> defaultScriptLanguage: aValue [
	"generated code"
	^ self propertyAt: #defaultScriptLanguage put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> headingLevelOffset [
	"generated code"
	^ self propertyAt: #headingLevelOffset ifAbsent: [0]
]

{ #category : #accessing }
ConfigurationForPillar >> headingLevelOffset: aValue [
	"generated code"
	^ self propertyAt: #headingLevelOffset put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> htmlChapterTemplate [
	"generated code"
	^ self propertyAt: #htmlChapterTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/html.mustache')]
]

{ #category : #accessing }
ConfigurationForPillar >> htmlChapterTemplate: aValue [
	"generated code"
	^ self propertyAt: #htmlChapterTemplate put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> htmlTemplate [
	"generated code"
	^ self propertyAt: #htmlTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/html.mustache')]
]

{ #category : #accessing }
ConfigurationForPillar >> htmlTemplate: aValue [
	"generated code"
	^ self propertyAt: #htmlTemplate put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> inputFile [
	"generated code"
	^ self propertyAt: #inputFile ifAbsent: [(FileSystem workingDirectory / 'book')]
]

{ #category : #accessing }
ConfigurationForPillar >> inputFile: aValue [
	"generated code"
	^ self propertyAt: #inputFile put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> keywords [
	"generated code"
	^ self propertyAt: #keywords ifAbsent: ['Pharo']
]

{ #category : #accessing }
ConfigurationForPillar >> keywords: aValue [
	"generated code"
	^ self propertyAt: #keywords put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> latexChapterTemplate [
	"generated code"
	^ self propertyAt: #latexChapterTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/chapter.latex.mustache')]
]

{ #category : #accessing }
ConfigurationForPillar >> latexChapterTemplate: aValue [
	"generated code"
	^ self propertyAt: #latexChapterTemplate put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> latexTemplate [
	"generated code"
	^ self propertyAt: #latexTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/main.latex.mustache')]
]

{ #category : #accessing }
ConfigurationForPillar >> latexTemplate: aValue [
	"generated code"
	^ self propertyAt: #latexTemplate put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> latexWriter [
	"generated code"
	^ self propertyAt: #latexWriter ifAbsent: ['latex:sbabook']
]

{ #category : #accessing }
ConfigurationForPillar >> latexWriter: aValue [
	"generated code"
	^ self propertyAt: #latexWriter put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> mainDocument [
	"generated code"
	^ self propertyAt: #mainDocument ifAbsent: [(FileSystem workingDirectory / 'book')]
]

{ #category : #accessing }
ConfigurationForPillar >> mainDocument: aValue [
	"generated code"
	^ self propertyAt: #mainDocument put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> newLine [
	"generated code"
	^ self propertyAt: #newLine ifAbsent: [UnixPlatform new lineEnding]
]

{ #category : #accessing }
ConfigurationForPillar >> newLine: aValue [
	"generated code"
	^ self propertyAt: #newLine put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> outputDirectory [
	"generated code"
	^ self propertyAt: #outputDirectory ifAbsent: [(FileSystem workingDirectory / 'build')]
]

{ #category : #accessing }
ConfigurationForPillar >> outputDirectory: aValue [
	"generated code"
	^ self propertyAt: #outputDirectory put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> outputFile [
	"generated code"
	^ self propertyAt: #outputFile ifAbsent: [(FileSystem workingDirectory / 'output')]
]

{ #category : #accessing }
ConfigurationForPillar >> outputFile: aValue [
	"generated code"
	^ self propertyAt: #outputFile put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> renderStructureAsSlide [
	"generated code"
	^ self propertyAt: #renderStructureAsSlide ifAbsent: [false]
]

{ #category : #accessing }
ConfigurationForPillar >> renderStructureAsSlide: aValue [
	"generated code"
	^ self propertyAt: #renderStructureAsSlide put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> scrambledEmailAddresses [
	"generated code"
	^ self propertyAt: #scrambledEmailAddresses ifAbsent: [false]
]

{ #category : #accessing }
ConfigurationForPillar >> scrambledEmailAddresses: aValue [
	"generated code"
	^ self propertyAt: #scrambledEmailAddresses put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> separateOutputFiles [
	"generated code"
	^ self propertyAt: #separateOutputFiles ifAbsent: [false]
]

{ #category : #accessing }
ConfigurationForPillar >> separateOutputFiles: aValue [
	"generated code"
	^ self propertyAt: #separateOutputFiles put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> series [
	"generated code"
	^ self propertyAt: #series ifAbsent: ['Square Bracket Associate Collection']
]

{ #category : #accessing }
ConfigurationForPillar >> series: aValue [
	"generated code"
	^ self propertyAt: #series put: aValue
]

{ #category : #accessing }
ConfigurationForPillar >> title [
	"generated code"
	^ self propertyAt: #title ifAbsent: ['my super cool book']
]

{ #category : #accessing }
ConfigurationForPillar >> title: aValue [
	"generated code"
	^ self propertyAt: #title put: aValue
]
