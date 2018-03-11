Class {
	#name : #ConfigurationForXX,
	#superclass : #ChrysalConfiguration,
	#category : #'Chrysal-Tests'
}

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

{ #category : #conversion }
ConfigurationForXX >> convertAttribution: aValue [
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
ConfigurationForXX >> convertVerbose: aValue [
	"generated code"
	^ Object readFrom: aValue
]

{ #category : #'declared items' }
ConfigurationForXX >> declaredItems [
	"generated code"
	^ #( #title #verbose #attribution #series #keywords #outputDirectory #mainDocument #latexTemplate #latexChapterTemplate #htmlTemplate #htmlChapterTemplate #newLine #latexWriter )
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

{ #category : #accessing }
ConfigurationForXX >> verbose [
	"generated code"
	^ self propertyAt: #verbose ifAbsent: [defaultValue]
]

{ #category : #accessing }
ConfigurationForXX >> verbose: aValue [
	"generated code"
	^ self propertyAt: #verbose put: aValue
]
