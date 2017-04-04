# Delphi MongoDB Driver

A Delphi driver to access a mongoDB server.
It uses [jsonDoc.pas](https://github.com/stijnsanders/jsonDoc#jsondoc) to store JSON documents. `IJSONDocument`  maps variables onto Delphi variables of type Variant, which resembles
the loose typing of JavaScript.
There are three main units and two main classes to enable access to a mongo DB server:

## jsonDoc.pas

Declares `IJSONDocument` and related interfaces, and the `JSON` function to create instances, optionally populated with data. `IJSONDocument` instances hold the data of a 'document', the basic unit of data mongoDB works with.
A variable of type Variant can hold an interface reference to an instance, which enables embedding documents.
Use Variant arrays (or `IJSONArray`) to store arrays of values in a document.

See also https://github.com/stijnsanders/jsonDoc#jsondoc

## bsonTools.pas

Declares the `LoadBSON` and `SaveBSON` procedures.

Also declares the `IBSONDocArray` interface which can improve processing arrays of embedded documents by keeping a reference to the underlying data stream, and only loading one document at a time, possibly re-using allocated memory for the same keys if the documents have a similar structure.
**Attention:** take care to keep the TStream instance in existance for as long as you're planning to use the linked `IBSONDocArray` instance. Failure to do so can lead to _privileged instruction_ or _access violation_ errors.

## mongoWire.pas

Use an object of class `TMongoWire` to connection to a mongoDB server. It supports getting single items, performing inserts, updates and deletes.

Use objects of class `TMongoWireQuery` to query to a mongoDB server. It handles the cursor and subsequent requests to the server to get more data when needed.

## mongoID.pas

Use function `mongoObjectId` to construct a new MongoDB-style id value.

## mongoAuth3.pas

Use procedure `MongoWireAuthenticate` to authenticate a newly connected `TMongoWire` instance. As of version 3.0 MongoDB uses a slightly modified `SCRAM-SHA-1` to vastly improve security with access control. (Use `mongoAuth.pas` for MongoDB versions prior to 3.0.)

## mongoStream.pas

Use `TMongoStream` to load and store files in MongoDB. Internally `.files` and `.chunks` collections are used to store the data.

## examples

See the example projects for straight-forward demonstration applications that use TMongoWire.
