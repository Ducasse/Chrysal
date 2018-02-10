Class {
	#name : #ConfigurationForXX,
	#superclass : #Configuration,
	#category : #Chrysal
}

{ #category : #accessing }
ConfigurationForXX >> mainDocument [
	"generated code"
	^ self propertyAt: #mainDocument ifAbsent: [(FileSystem workingDirectory / 'book' asFileReference)]
]

{ #category : #accessing }
ConfigurationForXX >> mainDocument: aProperty [
	"generated code"
	^ self propertyAt: #mainDocument put: aProperty
]

{ #category : #accessing }
ConfigurationForXX >> newline [
	"generated code"
	^ self propertyAt: #newline ifAbsent: [UnixPlatform new lineEnding]
]

{ #category : #accessing }
ConfigurationForXX >> newline: aProperty [
	"generated code"
	^ self propertyAt: #newline put: aProperty
]

{ #category : #accessing }
ConfigurationForXX >> outputDirectory [
	"generated code"
	^ self propertyAt: #outputDirectory ifAbsent: [(FileSystem workingDirectory / 'build')]
]

{ #category : #accessing }
ConfigurationForXX >> outputDirectory: aProperty [
	"generated code"
	^ self propertyAt: #outputDirectory put: aProperty
]

{ #category : #accessing }
ConfigurationForXX >> separateOutputFiles [
	"generated code"
	^ self propertyAt: #separateOutputFiles ifAbsent: [true]
]

{ #category : #accessing }
ConfigurationForXX >> separateOutputFiles: aProperty [
	"generated code"
	^ self propertyAt: #separateOutputFiles put: aProperty
]
