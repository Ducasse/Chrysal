Class {
	#name : #BooleanConfigurationItem,
	#superclass : #ConfigurationItem,
	#instVars : [
		'defaultDomainValue'
	],
	#category : #Chrysal
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
	defaultDomainValue := false
]

{ #category : #'public API' }
BooleanConfigurationItem >> beTrueAsDefault [

	defaultDomainValue := true
]

{ #category : #accessing }
BooleanConfigurationItem >> defaultDomainValue [
	^ defaultDomainValue ifNil: [ self beTrueAsDefault  ]
]

{ #category : #accessing }
BooleanConfigurationItem >> defaultDomainValueString [
	^ self defaultDomainValue asString
]

{ #category : #unused }
BooleanConfigurationItem >> possibleValuesString [
	^ #('true' 'false')
	
]
