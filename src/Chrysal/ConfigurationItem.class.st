Class {
	#name : #ConfigurationItem,
	#superclass : #Object,
	#instVars : [
		'propertyName'
	],
	#category : #Chrysal
}

{ #category : #accessing }
ConfigurationItem >> possibleValuesString [

	^ self subclassResponsibility 
]

{ #category : #accessing }
ConfigurationItem >> propertyName [
	^ propertyName
]

{ #category : #accessing }
ConfigurationItem >> propertyName: aString [
	"property name"

	propertyName := aString
]
