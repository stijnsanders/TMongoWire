unit TestBSONUtils;

interface

uses
  Dialogs,
  TestFramework,

  bsonDoc,
  bsonUtils;

type

  ///
  /// Include JSON data format entre {* *} in sample JSON bacauuse comflit with { } coments DElphi
  ///

  // test methods bsonUtils.pas
  TTestBSONUtils = class(TTestCase)
  private
    d: IBSONDocument;
    JSON: string;
  published
    procedure TestBasic();

    procedure Test02();
    procedure Test03();
    procedure Test04();
    procedure Test05();
  end;

implementation

uses
  Variants;

{ TTestBSONUtils }

procedure TTestBSONUtils.Test02;
begin
  d:= BSON([
    'Company','XYZ Company',
    'Address','XYZ Address',
    'Phones',VarArrayOf([
      BSON(['number','714-999-9999', 'type','business']),
      BSON(['number','714-987-6533', 'type','cell'])
    ])]);

  JSON:=
    '{"Company":"XYZ Company",' +
    '"Address":"XYZ Address",' +
    '"Phones":[' +
      '{"number":"714-999-9999","type":"business"},' +
      '{"number":"714-987-6533","type":"cell"}'+
    ']}';

  CheckEqualsString( JSON, BsonToJson( d ) );
end;

procedure TTestBSONUtils.Test03;
begin
  d:= BSON([ 'movies' ,
    VarArrayOf( [
      BSON([ 'title', 'Shut Up and Play the Hits' , 'year', 2012 ]),
      BSON([ 'title', 'John Morre no Final' , 'year', 2012 ]),
      BSON([ 'title', 'The Comedy' , 'year', 2012 ]),
      BSON([ 'title', 'Bem-vindo aos 40' , 'year', 2012 ]),
      BSON([ 'title', 'Cop Land' , 'year', 1997])
    ])
  ]);

  JSON:=
    '{"movies":[' +
       '{"title":"Shut Up and Play the Hits","year":2012},' +
       '{"title":"John Morre no Final","year":2012},' +
       '{"title":"The Comedy","year":2012},' +
       '{"title":"Bem-vindo aos 40","year":2012},' +
       '{"title":"Cop Land","year":1997}' +
    ']}';


  CheckEqualsString( JSON, BsonToJson( d ) );

end;

procedure TTestBSONUtils.Test04;
begin
  d:=BSON([
    'Name', 'Johni',
    'Sports', VarArrayOf( ['football', 'volleyball', 'chess'] )
  ]);

  JSON:=
    '{"Name":"Johni",' +
     '"Sports":["football","volleyball","chess"]' +
    '}';

  CheckEqualsString( JSON, BsonToJson( d ) );

end;

procedure TTestBSONUtils.Test05;
begin

  d := BSON([
    'name' , 'Johni Douglas Marangon',
    'age' , 27,
    'sex' , 'M',
    'salary' , 1500,
    'registered' , true,
    'favorites' , BSON([ 'color' , 'Red' , 'sport' , 'Soccer', 'food' , 'Spaghetti'] )
  ]);


  JSON :=
    '{"name":"Johni Douglas Marangon",' +
     '"age":27,' +
     '"sex":"M",' +
     '"salary":1500,' +
     '"registered":true,' +
     '"favorites":{' +
        '"color":"Red",' +
        '"sport":"Soccer",' +
        '"food":"Spaghetti"' +
       '}' +
     '}';

  CheckEqualsString(JSON, BsonToJson(d));

end;

procedure TTestBSONUtils.TestBasic;
begin
  d := BSON(['name', 'Johni Douglas Marangon', 'address',
    'Maravilia - Santa Catarina - Brazil', 'phone', 4895959859]);

  JSON :=
    '{"name":"Johni Douglas Marangon","address":"Maravilia - Santa Catarina - Brazil","phone":4895959859}';

  CheckEqualsString(JSON, BsonToJson(d));



  (*

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
*)
end;

initialization

RegisterTest(TTestBSONUtils.Suite);

end.
