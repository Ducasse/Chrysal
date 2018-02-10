Class {
	#name : #NewLineConfigurationItem,
	#superclass : #ConfigurationItem,
	#instVars : [
		'defaultDomainObject'
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

{ #category : #accessing }
NewLineConfigurationItem >> defaultDomainValue [
	^ defaultDomainObject 
]

{ #category : #'as yet unclassified' }
NewLineConfigurationItem >> defaultIsUnix [
	defaultDomainObject := self mapperFromUserToDomain at: #unix
]

{ #category : #'as yet unclassified' }
NewLineConfigurationItem >> mapperFromUserToDomain [
	
	| fromUserToDomainDictionary|
	fromUserToDomainDictionary := self mapperFromUserToDomainRepresentations copy. 
	fromUserToDomainDictionary keysAndValuesDo: [ :k :v | 
		fromUserToDomainDictionary at: k put: (fromUserToDomainDictionary at: k put: (Compiler evaluate: v))
		].
 	^ fromUserToDomainDictionary
]

{ #category : #'as yet unclassified' }
NewLineConfigurationItem >> mapperFromUserToDomainRepresentations [

	 ^ (Dictionary
			newFrom:
				{#cr -> 'String cr' .
				#lf -> 'UnixPlatform new lineEnding'.
				#crlf -> 'Win32Platform new lineEnding'.
				#unix -> 'UnixPlatform new lineEnding'.
				#mac -> 'UnixPlatform new lineEnding'.
				#dos -> 'Win32Platform new lineEnding'})
]

{ #category : #accessing }
NewLineConfigurationItem >> possibleValuesString [
	^ #(#cr #lf  #crlf  #unix  #mac  #dos)
]
