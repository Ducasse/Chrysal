"
I'm a dummy class to hold some items (configuration items). 
Now Chrysal does not need it. 
"
Class {
	#name : #ChrysalConfigurationDescription,
	#superclass : #Object,
	#instVars : [
		'items'
	],
	#category : #'Chrysal-Model'
}

{ #category : #sample }
ChrysalConfigurationDescription class >> itemDescriptionForPillar [
	^ {(StringConfigurationItem new
		propertyName: 'title';
		default: 'my super cool book';
		yourself).
	(StringConfigurationItem new
		propertyName: 'attribution';
		default: 'me, myself and I';
		yourself).
	(StringConfigurationItem new
		propertyName: 'series';
		default: 'Square Bracket Associate Collection';
		yourself).
	(StringConfigurationItem new
		propertyName: 'keywords';
		default: 'Pharo';
		yourself).
	(FolderConfigurationItem new
		propertyName: 'outputDirectory';
		default: 'build').
	(FileConfigurationItem new
		propertyName: 'mainDocument';
		default: 'book').
	(FileConfigurationItem new
		propertyName: 'latexTemplate';
		default: '_support/templates/main.latex.mustache').
	(FileConfigurationItem new
		propertyName: 'latexChapterTemplate';
		default: '_support/templates/chapter.latex.mustache').
	(FileConfigurationItem new
		propertyName: 'htmlTemplate';
		default: '_support/templates/html.mustache').
	(FileConfigurationItem new
		propertyName: 'htmlChapterTemplate';
		default: '_support/templates/html.mustache').
	(NewLineConfigurationItem new
		propertyName: 'newLine';
		defaultIsUnix).
	(StringConfigurationItem new
		propertyName: 'latexWriter';
		default: #latex:sbabook;
		yourself)	"may be we should turn it into a Pillar specific item that convert to a specific class"}
]

{ #category : #sample }
ChrysalConfigurationDescription class >> itemDescriptionForXX [

	^ {(StringConfigurationItem new
		propertyName: 'title';
		default: 'my super cool book';
		yourself).
	(BooleanConfigurationItem new
		propertyName: 'verbose';
		beTrueAsDefault;
		yourself).
	(StringConfigurationItem new
		propertyName: 'attribution';
		default: 'me, myself and I';
		yourself).
	(StringConfigurationItem new
		propertyName: 'series';
		default: 'Square Bracket Associate Collection';
		yourself).
	(StringConfigurationItem new
		propertyName: 'keywords';
		default: 'Pharo';
		yourself).
	(FolderConfigurationItem new
		propertyName: 'outputDirectory';
		default: 'build').
	(FileConfigurationItem new
		propertyName: 'mainDocument';
		default: 'book').
	(FileConfigurationItem new
		propertyName: 'latexTemplate';
		default: '_support/templates/main.latex.mustache').
	(FileConfigurationItem new
		propertyName: 'latexChapterTemplate';
		default: '_support/templates/chapter.latex.mustache').
	(FileConfigurationItem new
		propertyName: 'htmlTemplate';
		default: '_support/templates/html.mustache').
	(FileConfigurationItem new
		propertyName: 'htmlChapterTemplate';
		default: '_support/templates/html.mustache').
	(NewLineConfigurationItem new
		propertyName: 'newLine';
		defaultIsUnix).
	(SymbolConfigurationItem new
		propertyName: 'latexWriter';
		default: #latex:sbabook;
		yourself)	"may be we should turn it into a Pillar specific item that convert to a specific class"}
]
