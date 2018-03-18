Class {
	#name : #BooleanConfigurationItem,
	#superclass : #ChrysalItem,
	#instVars : [
		'defaultDomainValue'
	],
	#category : #'Chrysal-Model'
}

{ #category : #example }
BooleanConfigurationItem class >> example [
	<sampleInstance>
	^ BooleanConfigurationItem new
		propertyName: 'separateOutputFiles';
		beTrueAsDefault
]

{ #category : #'public API' }
BooleanConfigurationItem >> beFalseAsDefault [
	default := 'false'
]

{ #category : #'public API' }
BooleanConfigurationItem >> beTrueAsDefault [
	default := 'true'
]

{ #category : #unused }
BooleanConfigurationItem >> convertToDomainObject: aValue [
	"self new convertToDomainObject: 'true'"
	^ Object readFrom: aValue
]

{ #category : #accessing }
BooleanConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ self defaultValue
]

{ #category : #'public API' }
BooleanConfigurationItem >> defaultValue [
	"Default value is the default value of an item in the domain of the configuration writer and not in the Pharo world."
	^ default
]

{ #category : #accessing }
BooleanConfigurationItem >> domainValueConversionString [
	"Returns a string representing the conversion of a value from the configuration writer world to Pharo. aValue is the parameter representing the object to be converted"

	^ 'Object readFrom: aValue'
]

{ #category : #unused }
BooleanConfigurationItem >> possibleValuesString [
	^ #('true' 'false')
	
]
