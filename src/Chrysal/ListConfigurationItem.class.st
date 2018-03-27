"
I represent an item holding a list of other elements. 


(ListConfigurationItem new
		propertyName: 'levels';
		default: #OrderedCollection;
		elements: #(printerComposite);
		yourself)
"
Class {
	#name : #ListConfigurationItem,
	#superclass : #ChrysalItem,
	#instVars : [
		'elements'
	],
	#category : #'Chrysal-Model'
}

{ #category : #accessing }
ListConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ ' {1} new ' format: {self default asString}
]

{ #category : #accessing }
ListConfigurationItem >> elements: aCollection [ 
	elements := aCollection
]

{ #category : #initialization }
ListConfigurationItem >> initialize [

	super initialize. 
	elements := OrderedCollection new.
]
