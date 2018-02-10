Class {
	#name : #ConfigurationItem,
	#superclass : #Object,
	#instVars : [
		'propertyName',
		'default'
	],
	#category : #Chrysal
}

{ #category : #accessing }
ConfigurationItem >> default [
	^ default ifNil: [ default := self defaultValue ]
]

{ #category : #accessing }
ConfigurationItem >> default: aString [ 
	default := aString
]

{ #category : #accessing }
ConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ self subclassResponsibility
]

{ #category : #accessing }
ConfigurationItem >> defaultValue [
	"Default value is the default value of an item in the domain of the configuration writer and not in the Pharo world."
	^ 'defaultValue'
]

{ #category : #unused }
ConfigurationItem >> possibleValuesString [
	^ ''
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
