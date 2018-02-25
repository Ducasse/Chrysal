Class {
	#name : #NewLineConfigurationItem,
	#superclass : #Chrysal,
	#instVars : [
		'defaultDomainObject',
		'defaultKey'
	],
	#category : #Chrysal
}

{ #category : #example }
NewLineConfigurationItem class >> example [
	<sampleInstance>
	
	^ NewLineConfigurationItem new
		propertyName: 'newline';
		defaultIsUnix
]

{ #category : #unused }
NewLineConfigurationItem >> convertToDomainObject: aValue [
	"not good because I cannot generate it in the configuration class code"
	^ self mapperFromUserToDomain at: aValue
]

{ #category : #accessing }
NewLineConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ self mapperFromUserToDomainRepresentations at: self default
]

{ #category : #'public api' }
NewLineConfigurationItem >> defaultIsUnix [
	defaultKey := #unix.
	
]

{ #category : #accessing }
NewLineConfigurationItem >> defaultValue [
	^ #unix
]

{ #category : #conversion }
NewLineConfigurationItem >> domainValueConversionString [
	"Returns a string converting a string as written by in the configuration file to a pharo object. 
	Note that this method is like the body of a macro that will be expanded in the configuration class: 
	here aValue is the name of the parameter of the generated method.
	"

	^ '(Dictionary
			newFrom:
				{(#cr -> String cr).
				(#lf -> UnixPlatform new lineEnding).
				(#crlf -> Win32Platform new lineEnding).
				(#unix -> UnixPlatform new lineEnding).
				(#mac -> UnixPlatform new lineEnding).
				(#dos -> Win32Platform new lineEnding)}) at: aValue'
]

{ #category : #'mapping logic' }
NewLineConfigurationItem >> mapperFromUserToDomain [
	"coded this way because we may inline it in configuration class."
	| fromUserToDomainDictionary |
	fromUserToDomainDictionary := self mapperFromUserToDomainRepresentations copy.
	fromUserToDomainDictionary
		keysAndValuesDo: [ :k :v | 
			fromUserToDomainDictionary
				at: k
				put: (fromUserToDomainDictionary at: k put: (Compiler evaluate: v)) ].
	^ fromUserToDomainDictionary
]

{ #category : #'mapping logic' }
NewLineConfigurationItem >> mapperFromUserToDomainRepresentations [
	"coded this way because we may inline it in configuration class."
	^ Dictionary
		newFrom:
			{(#cr -> 'String cr').
			(#lf -> 'UnixPlatform new lineEnding').
			(#crlf -> 'Win32Platform new lineEnding').
			(#unix -> 'UnixPlatform new lineEnding').
			(#mac -> 'UnixPlatform new lineEnding').
			(#dos -> 'Win32Platform new lineEnding')}
]

{ #category : #unused }
NewLineConfigurationItem >> possibleValuesString [
	^ #(#cr #lf  #crlf  #unix  #mac  #dos)
]
