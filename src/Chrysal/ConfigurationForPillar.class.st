Class {
	#name : #ConfigurationForPillar,
	#superclass : #ChrysalConfiguration,
	#category : #'Chrysal-ToBeMovedElsewhere'
}

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
	^ #( #title #attribution #series #keywords #outputDirectory #mainDocument #latexTemplate #latexChapterTemplate #htmlTemplate #htmlChapterTemplate #newLine #latexWriter )
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
