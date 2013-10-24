unit TestBSONUtils;

interface

uses
  Dialogs,
  TestFramework;

type

///
/// Include JSON data format entre {* *} in sample JSON bacauuse comflit with { } coments DElphi
///


  // test methods bsonUtils.pas
  TTestBSONUtils = class( TTestCase )
  published
    procedure TestBasic();
  end;

implementation

uses
  Variants,

  bsonDoc, bsonUtils;

{ TTestBSONUtils }

procedure TTestBSONUtils.TestBasic;
var
  d: IBSONDocument;


begin
(*
{
   "id":"ObjectID(\"526735c535f5d5052013925f\")",
   "name":"Johni Douglas Marangon",
   "address":"Maravilia - Santa Catarina - Brazil",
   "phone":"4895959859"
}
*)

  d:= BSON([
    'name' , 'Johni Douglas Marangon',
    'address' , 'Maravilia - Santa Catarina - Brazil',
    'phone' , '4895959859'
  ] );


  d:= BSON([
  'Company','XYZ Company',
  'address','XYZ Address',
  'Phones',VarArrayOf([
    BSON(['number','714-999-9999', 'type','business']),
    BSON(['number','714-987-6533', 'type','cell'])
  ])]);

  d:= BSON([ 'movies' ,
    VarArrayOf( [
      BSON([ 'title', 'Shut Up and Play the Hits' , 'year ', '2012' ]),
      BSON([ 'title', 'John Morre no Final' , 'year ', '2012' ]),
      BSON([ 'title', 'The Comedy' , 'year ', '2012' ]),
      BSON([ 'title', 'Bem-vindo aos 40 ' , 'year ', '2012' ]),
      BSON([ 'title', 'Cop Land' , 'year ', '1997' ])
    ])
  ]);
  ShowMessage( BsonToJson( d ) );

//  http://www.jsonexample.com/

 (*
  newObject = {
 "first": "John",
 "last": "Doe",
 "age": 39,
 "sex": "M",
 "salary": 70000,
 "registered": true,
 "interests": [ "Reading", "Mountain Biking", "Hacking" ]
}
*)




newObject = {
 "first": "John",
 "last": "Doe",
 "age": 39,
 "sex": "M",
 "salary": 70000,
 "registered": true,
 "favorites": {
  "color": "Blue",
  "sport": "Soccer",
  "food": "Spaghetti"
 }
}

  d:= BSON([
    'firstName' ,  'Johni' ,
    'address' , 'Maravilia',
    'salay' , '1000',
    'actived' , 'true',
    'sex' , 'M',

    'favorits' , VarArrayOf

  ])




newObject = {
 "first": "John",
 "last": "Doe",
 "age": 39,
 "sex": "M",
 "salary": 70000,
 "registered": true,
 "interests": [ "Reading", "Mountain Biking", "Hacking" ],
 "favorites": {
  "color": "Blue",
  "sport": "Soccer",
  "food": "Spaghetti"
 },
 "skills": [
  {
   "category": "PHP",
   "tests": [
    { "name": "One", "score": 90 },
    { "name": "Two", "score": 96 }
   ]
  },
  {
   "category": "CouchDB",
   "tests": [
    { "name": "One", "score": 32 },
    { "name": "Two", "score": 84 }
   ]
  },
  {
   "category": "Node.js",
   "tests": [
    { "name": "One", "score": 97 },
    { "name": "Two", "score": 93 }
   ]
  }
 ]
}



//var
//  d:IBSONDocument;
d:=BSON([
    'id',mongoObjectID,
    'sentence', 'this is a sentece',
    'tags','["some", "indexing", "words"]'
]);
FMongoWire.Insert(theCollection,d);


//var
//q:TMongoWireQuery;
//qb:IBSONDocument
qb:=BSON(['tags', '"words"']); //***
q:=TMongoWireQuery.Create(FMongoWire);
q.Query(mwx2Collection, qb); //***


d:=BSON([
    'id',mongoObjectID,
    'sentence', 'this is a sentece',
    'tags',VarArrayOf(['some', 'indexing', 'words'])
]);
FMongoWire.Insert(theCollection,d);,








*)

end;

initialization
  RegisterTest( TTestBSONUtils.Suite );

end.















