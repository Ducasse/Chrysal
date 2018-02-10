Class {
	#name : #ConfigurationForXX,
	#superclass : #Configuration,
	#category : #Chrysal
}

{ #category : #accessing }
ConfigurationForXX >> separateOutputFiles [
	"generated code"
	^ self propertyAt: #separateOutputFiles ifAbsent: [true]
]
