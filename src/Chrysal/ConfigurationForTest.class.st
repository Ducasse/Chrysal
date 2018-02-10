Class {
	#name : #ConfigurationForTest,
	#superclass : #Configuration,
	#category : #Chrysal
}

{ #category : #accessing }
ConfigurationForTest >> mainDocument [
	"generated code"
	^ self propertyAt: #mainDocument ifAbsent: [(FileSystem workingDirectory / 'book' asFileReference)]
]

{ #category : #accessing }
ConfigurationForTest >> mainDocument: aProperty [
	"generated code"
	^ self propertyAt: #mainDocument put: aProperty
]

{ #category : #accessing }
ConfigurationForTest >> newline [
	"generated code"
	^ self propertyAt: #newline ifAbsent: [UnixPlatform new lineEnding]
]

{ #category : #accessing }
ConfigurationForTest >> newline: aProperty [
	"generated code"
	^ self propertyAt: #newline put: aProperty
]

{ #category : #accessing }
ConfigurationForTest >> outputDirectory [
	"generated code"
	^ self propertyAt: #outputDirectory ifAbsent: [(FileSystem workingDirectory / 'build')]
]

{ #category : #accessing }
ConfigurationForTest >> outputDirectory: aProperty [
	"generated code"
	^ self propertyAt: #outputDirectory put: aProperty
]

{ #category : #accessing }
ConfigurationForTest >> separateOutputFiles [
	"generated code"
	^ self propertyAt: #separateOutputFiles ifAbsent: [true]
]

{ #category : #accessing }
ConfigurationForTest >> separateOutputFiles: aProperty [
	"generated code"
	^ self propertyAt: #separateOutputFiles put: aProperty
]
