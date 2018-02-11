Class {
	#name : #StringConfigurationItem,
	#superclass : #ConfigurationItem,
	#category : #Chrysal
}

{ #category : #example }
StringConfigurationItem class >> example [

	<sampleInstance>
	
	^ StringConfigurationItem new
		propertyName: 'title';
		default: 'my super cool book';
		yourself.
]

{ #category : #'mapping logic' }
StringConfigurationItem >> convertToDomainObject: aValue [ 
	^ aValue
]

{ #category : #accessing }
StringConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."
	^ self default
]

{ #category : #accessing }
StringConfigurationItem >> defaultValue [
	"default value for the default value of the item expressed in the domain of the configuration writer"
	^ 'default value for ' , self propertyName
]
