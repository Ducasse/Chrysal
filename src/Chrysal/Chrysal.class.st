Class {
	#name : #Chrysal,
	#superclass : #Object,
	#instVars : [
		'propertyName',
		'default'
	],
	#category : #Chrysal
}

{ #category : #accessing }
Chrysal >> default [
	^ default ifNil: [ default := self defaultValue ]
]

{ #category : #accessing }
Chrysal >> default: aString [ 
	default := aString
]

{ #category : #accessing }
Chrysal >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ self subclassResponsibility
]

{ #category : #accessing }
Chrysal >> defaultValue [
	"Default value is the default value of an item in the domain of the configuration writer and not in the Pharo world."
	^ 'defaultValue'
]

{ #category : #unused }
Chrysal >> possibleValuesString [
	^ ''
]

{ #category : #accessing }
Chrysal >> propertyName [
	^ propertyName
]

{ #category : #accessing }
Chrysal >> propertyName: aString [
	"property name"

	propertyName := aString
]
