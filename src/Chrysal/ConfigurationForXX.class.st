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
ConfigurationForXX >> newline [
	"generated code"
	^ self propertyAt: #newline ifAbsent: [UnixPlatform new lineEnding]
]

{ #category : #accessing }
ConfigurationForXX >> outputDirectory [
	"generated code"
	^ self propertyAt: #outputDirectory ifAbsent: [(FileSystem workingDirectory / 'build')]
]

{ #category : #accessing }
ConfigurationForXX >> separateOutputFiles [
	"generated code"
	^ self propertyAt: #separateOutputFiles ifAbsent: [true]
]
