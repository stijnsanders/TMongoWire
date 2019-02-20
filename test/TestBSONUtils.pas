// Test methods bsonUtils.pas
unit TestBSONUtils;

interface

uses
  TestFramework,
  bsonDoc,
  bsonUtils;

type
  TTestBSONUtils_BsonToJson = class(TTestCase)
  private
    d: IBSONDocument;
    JSON: string;
  published
    procedure TestBsonToJsonBasicStructure();

    procedure TestBsonToJsonComplex02();
    procedure TestBsonToJsonComplex03();
    procedure TestBsonToJsonComplex04();
    procedure TestBsonToJsonComplex05();
    procedure TestBsonToJsonComplex06();
    procedure TestBsonToJsonComplex07();
  end;

  TTestBSONUtils_JsonToBson = class(TTestCase)
  end;
  TTestBSONUtils_JsonIntoBson = class(TTestCase)
  end;


implementation

uses
  Variants,
  SysUtils;

{ TTestBSONUtils }

procedure TTestBSONUtils_BsonToJson.TestBsonToJsonComplex02;
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

procedure TTestBSONUtils_BsonToJson.TestBsonToJsonComplex03;
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

procedure TTestBSONUtils_BsonToJson.TestBsonToJsonComplex04;
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

procedure TTestBSONUtils_BsonToJson.TestBsonToJsonComplex05;
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

procedure TTestBSONUtils_BsonToJson.TestBsonToJsonBasicStructure;
begin
  d :=
    BSON([
      'Name', 'Johni Douglas Marangon',
      'Phone', 4912345678,
      'Sex' , 'M',
      'BirthDate', StrToDate( '28/06/1986' ),
      'Registered', True
    ]);

  JSON :=
    '{' +
      '"Name":"Johni Douglas Marangon",' +
      '"Phone":4912345678,' +
      '"Sex":"M",' +
      '"BirthDate":"1986-06-28T00:00:00.000",' +
      '"Registered":true' +

    '}';

  CheckEqualsString(JSON, BsonToJson(d));

end;


procedure TTestBSONUtils_BsonToJson.TestBsonToJsonComplex07;
begin
  d:= BSON([
     'test',
      VarArrayOf([
        BSON ([ 'category', 'PHP', 'result', VarArrayOf( [ BSON([ 'name' ,'One', 'score', 90 ]), BSON([ 'name' ,'Two', 'score', 75 ]) ] ) ] ),
        BSON ([ 'category', 'Delphi', 'result', VarArrayOf( [ BSON([ 'name' ,'One', 'score', 96 ]), BSON([ 'name' ,'Two', 'score', 52 ]) ] ) ] ),
        BSON ([ 'category', 'Java', 'result', VarArrayOf( [ BSON([ 'name' ,'One', 'score', 74 ]), BSON([ 'name' ,'Two', 'score', 49 ]) ] ) ] )
      ])
  ]);

JSON:=
  '{'+
    '"test":[' +
     '{"category":"PHP",' +
            '"result":[{"name":"One","score":90},' +
                      '{"name":"Two","score":75}]},' +
     '{"category":"Delphi",' +
            '"result":[{"name":"One","score":96},' +
                      '{"name":"Two","score":52}]},' +
     '{"category":"Java",'+
            '"result":[{"name":"One","score":74},' +
                      '{"name":"Two","score":49}]}' +
    ']' +
  '}';

  CheckEqualsString( JSON, BsonToJson( d ) );

end;

procedure TTestBSONUtils_BsonToJson.TestBsonToJsonComplex06;
begin
  d:= BSON( [
    'menu',
     BSON(
       [ 'header' ,'Viewer' ,
         'itens',  VarArrayOf([
             BSON([ 'id', 'Open' ]),
             BSON([ 'id', 'OpenNew', 'label', 'Open New' ]),
             BSON([ 'id', 'ZoonIn', 'label',  'Zoon In' , 'position', 10 ])
         ])
     ])
  ]);

  JSON:=
    '{"menu":{"header":"Viewer",' +
     '"itens":[' +
       '{"id":"Open"},' +
       '{"id":"OpenNew","label":"Open New"},' +
       '{"id":"ZoonIn","label":"Zoon In","position":10}' +
     ']'+
   '}}';

   CheckEqualsString( JSON, BsonToJson( d ) );

end;

initialization

RegisterTest(TTestBSONUtils_BsonToJson.Suite);

end.






