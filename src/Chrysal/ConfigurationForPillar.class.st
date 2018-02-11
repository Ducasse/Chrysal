Class {
	#name : #ConfigurationForPillar,
	#superclass : #Configuration,
	#category : #Chrysal
}

{ #category : #accessing }
ConfigurationForPillar >> attribution [
	"generated code"
	^ self propertyAt: #attribution ifAbsent: [me, myself and I]
]

{ #category : #accessing }
ConfigurationForPillar >> attribution: aProperty [
	"generated code"
	^ self propertyAt: #attribution put: aProperty
]

{ #category : #accessing }
ConfigurationForPillar >> htmlChapterTemplate [
	"generated code"
	^ self propertyAt: #htmlChapterTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/html.mustache' asFileReference)]
]

{ #category : #accessing }
ConfigurationForPillar >> htmlChapterTemplate: aProperty [
	"generated code"
	^ self propertyAt: #htmlChapterTemplate put: aProperty
]

{ #category : #accessing }
ConfigurationForPillar >> htmlTemplate [
	"generated code"
	^ self propertyAt: #htmlTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/html.mustache' asFileReference)]
]

{ #category : #accessing }
ConfigurationForPillar >> htmlTemplate: aProperty [
	"generated code"
	^ self propertyAt: #htmlTemplate put: aProperty
]

{ #category : #accessing }
ConfigurationForPillar >> keywords [
	"generated code"
	^ self propertyAt: #keywords ifAbsent: [Pharo]
]

{ #category : #accessing }
ConfigurationForPillar >> keywords: aProperty [
	"generated code"
	^ self propertyAt: #keywords put: aProperty
]

{ #category : #accessing }
ConfigurationForPillar >> latexChapterTemplate [
	"generated code"
	^ self propertyAt: #latexChapterTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/chapter.latex.mustache' asFileReference)]
]

{ #category : #accessing }
ConfigurationForPillar >> latexChapterTemplate: aProperty [
	"generated code"
	^ self propertyAt: #latexChapterTemplate put: aProperty
]

{ #category : #accessing }
ConfigurationForPillar >> latexTemplate [
	"generated code"
	^ self propertyAt: #latexTemplate ifAbsent: [(FileSystem workingDirectory / '_support/templates/main.latex.mustache' asFileReference)]
]

{ #category : #accessing }
ConfigurationForPillar >> latexTemplate: aProperty [
	"generated code"
	^ self propertyAt: #latexTemplate put: aProperty
]

{ #category : #accessing }
ConfigurationForPillar >> mainDocument [
	"generated code"
	^ self propertyAt: #mainDocument ifAbsent: [(FileSystem workingDirectory / 'book' asFileReference)]
]

{ #category : #accessing }
ConfigurationForPillar >> mainDocument: aProperty [
	"generated code"
	^ self propertyAt: #mainDocument put: aProperty
]

{ #category : #accessing }
ConfigurationForPillar >> newLine [
	"generated code"
	^ self propertyAt: #newLine ifAbsent: [UnixPlatform new lineEnding]
]

{ #category : #accessing }
ConfigurationForPillar >> newLine: aProperty [
	"generated code"
	^ self propertyAt: #newLine put: aProperty
]

{ #category : #accessing }
ConfigurationForPillar >> outputDirectory [
	"generated code"
	^ self propertyAt: #outputDirectory ifAbsent: [(FileSystem workingDirectory / 'build')]
]

{ #category : #accessing }
ConfigurationForPillar >> outputDirectory: aProperty [
	"generated code"
	^ self propertyAt: #outputDirectory put: aProperty
]

{ #category : #accessing }
ConfigurationForPillar >> series [
	"generated code"
	^ self propertyAt: #series ifAbsent: [Square Bracket Associate Collection]
]

{ #category : #accessing }
ConfigurationForPillar >> series: aProperty [
	"generated code"
	^ self propertyAt: #series put: aProperty
]

{ #category : #accessing }
ConfigurationForPillar >> title [
	"generated code"
	^ self propertyAt: #title ifAbsent: [my super cool book]
]

{ #category : #accessing }
ConfigurationForPillar >> title: aProperty [
	"generated code"
	^ self propertyAt: #title put: aProperty
]
