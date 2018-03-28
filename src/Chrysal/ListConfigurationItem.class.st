"
I represent an item holding a list of other elements. 


(ListConfigurationItem new
		propertyName: 'levels';
		default: #OrderedCollection;
		element: #printerComposite;
		yourself)
		
	
I handle the following 


{

''levels'' : [
		[    ''level'' : 1,
			''renderAs'' : ''letter'',
			''capitalization'' : ''uppercase'',
			''numbering'' : true,
			''size'' : 2
		],
		[	''level'' : 2,
			''renderAs'' : ''number'',
			''capitalization'' : ''uppercase'',
			''numbering'' : true,
			''size'' : 3
		]
	]

}
"
Class {
	#name : #ListConfigurationItem,
	#superclass : #ChrysalItem,
	#instVars : [
		'element'
	],
	#category : #'Chrysal-Model'
}

{ #category : #accessing }
ListConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ ' {1} new ' format: {self default asString}
]

{ #category : #accessing }
ListConfigurationItem >> domainValueConversionString [
	"Returns a string converting a string as written in the configuration file to a pharo object. 
	Note that this method is like the body of a macro that will be expanded in the configuration class: here aValue is the name of the parameter of the generated method.
	"
	^ '| elements |
	elements := {1} new.
	aValue do: [:v | 
		 elements add: (self convert{2}: v ) ].
	^ elements' format: {self default asString . element capitalized}
]

{ #category : #accessing }
ListConfigurationItem >> element: aCollection [ 
	element := aCollection
]

{ #category : #initialization }
ListConfigurationItem >> initialize [

	super initialize. 
	elements := OrderedCollection new.
]
