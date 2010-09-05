{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2009 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jan 7, 2010
*******************************************************************************
 XML generator. Don't use it! :)
******************************************************************************}
unit BigXML;

interface

uses
  SysUtils, Contnrs, Classes, RTLConsts;

type
  TXMLNodeList = class;

  TOperation = (noInsert, noDelete);

  TXMLNode = class
  private
    FChildren: TXMLNodeList;
    FParent: TXMLNode;
    FText: String;
    FElement: String;
    FAttributes: TStringList;
    function GetAttributes: TStrings;
    function GetAttributesStr: String;
    function GetChildren: TXMLNodeList;
    procedure SetAttributesStr(const Value: String);
    procedure Notification(Node: TXMLNode; Operation: TOperation);
  public
    constructor Create(AParent: TXMLNode);
    destructor Destroy; override;

    procedure Delete;

    property Element: String read FElement write FElement;
    property Text: String read FText write FText;

    procedure AddAttribute(const Name, Value: String);
    property AttributesStr: String read GetAttributesStr write SetAttributesStr;

    property Children: TXMLNodeList read GetChildren;

    function InnerXml: String; virtual;
    function OuterXml: String; virtual;
  end;

  TXMLDocument = class
  private
    FRootNode: TXMLNode;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property RootNode: TXMLNode read FRootNode;

    function XML: String; virtual;
  end;


  TXMLNodeList = class
  private
    FNodeList: TObjectList;
    FParent: TXMLNode;
    function GetNodeCount: Integer;
    function GetNode(Index: Integer): TXMLNode;
    procedure InsertNode(Node: TXMLNode);
    procedure RemoveNode(Node: TXMLNode);
    procedure Clear;
  public
    constructor Create(AParent: TXMLNode);
    destructor Destroy; override;
    property Nodes[Index: Integer]: TXMLNode read GetNode; default;
    property NodeCount: Integer read GetNodeCount;

    function Add( const Element: string;
                  const Attributes: string = '';
                  const Text: String = ''): TXMLNode;
  end;

implementation

uses Math;

{ TXMLNodeList }

function TXMLNodeList.Add(const Element, Attributes, Text: String): TXMLNode;
begin
  Result := TXMLNode.Create(FParent);
  Result.Element := Element;
  Result.SetAttributesStr(Attributes);
  Result.Text := Text;

  FParent.Notification(Result, noInsert);
end;

constructor TXMLNodeList.Create(AParent: TXMLNode);
begin
  inherited Create;

  FParent := AParent;
end;

destructor TXMLNodeList.Destroy;
begin
  Clear;

  inherited;
end;

function TXMLNodeList.GetNodeCount: Integer;
begin
  if Assigned(FNodeList) then
    Result := FNodeList.Count
  else
    Result := 0;
end;

function TXMLNodeList.GetNode(Index: Integer): TXMLNode;
begin
  if (Index < 0) or (Index >= GetNodeCount) then
    raise EListError.CreateRes(@SListIndexError);

  Result := TXMLNode(FNodeList[Index]);
end;


procedure TXMLNodeList.InsertNode(Node: TXMLNode);
begin
  if GetNodeCount = 0 then
    FNodeList := TObjectList.Create(False);
  FNodeList.Add(Node);
end;

procedure TXMLNodeList.RemoveNode(Node: TXMLNode);
begin
  // Check? if GetNodeCount = 0 then
  Node.FParent := nil;

  FNodeList.Remove(Node);
  if FNodeList.Count = 0 then
    FreeAndNil(FNodeList);
end;

procedure TXMLNodeList.Clear;
var
  i: Integer;
begin
  for i := GetNodeCount - 1 downto 0 do
    TXMLNode(FNodeList[i]).Free;
end;

{ TXMLNode }

procedure TXMLNode.AddAttribute(const Name, Value: String);
begin
  if Value = '' then
    GetAttributes.Values[Name] := Name
  else
    GetAttributes.Values[Name] := Value;
end;

constructor TXMLNode.Create(AParent: TXMLNode);
begin
  inherited Create;

  FParent := AParent;
  FChildren := TXMLNodeList.Create(Self);
end;

procedure TXMLNode.Delete;
begin
  Free;
end;

destructor TXMLNode.Destroy;
begin
  if Assigned(FParent) then
    FParent.Notification(Self, noDelete);

  FChildren.Free;
  FAttributes.Free;

  inherited;
end;

function TXMLNode.GetAttributes: TStrings;
begin
  if not Assigned(FAttributes) then
  begin
    FAttributes := TStringList.Create;
    FAttributes.Delimiter := ' ';
  end;
  Result := FAttributes;
end;

function TXMLNode.GetAttributesStr: String;
begin
  Result := '';
  if Assigned(FAttributes) then
    Result := FAttributes.DelimitedText;
end;

function TXMLNode.GetChildren: TXMLNodeList;
begin
  Result := FChildren;
end;

function TXMLNode.InnerXml: String;
var
  i: Integer;
begin
  // This might need some optimization, stringbuilder-like
  Result := FText;
  for i := 0 to FChildren.GetNodeCount - 1 do
    Result := Result + FChildren[i].OuterXml;
end;

procedure TXMLNode.Notification(Node: TXMLNode; Operation: TOperation);
begin
  case Operation of
    noInsert: FChildren.InsertNode(Node);
    noDelete: FChildren.RemoveNode(Node);
  end;
end;

function TXMLNode.OuterXml: String;
var
  Inner: String;
  Attr: String;
  i: Integer;
begin
  Inner := InnerXML;
  Attr := '';
  if Assigned(FAttributes) then
    for i := 0 to FAttributes.Count - 1 do
      Attr := Attr + ' ' + FAttributes.Names[i] + '="' + FAttributes.ValueFromIndex[i] + '"';

  if Length(Inner) = 0 then
    Result := '<' + Element + Attr + '/>'
  else
    Result := '<' + Element + Attr + '>' + Inner + '</' + Element + '>';
end;

procedure TXMLNode.SetAttributesStr(const Value: String);
begin
  if Value <> '' then
    GetAttributes.DelimitedText := Value;
end;

{ TXMLDocument }

constructor TXMLDocument.Create;
begin
  inherited Create;

  FRootNode := TXMLNode.Create(nil);
end;

destructor TXMLDocument.Destroy;
begin
  FRootnode.Free;

  inherited;
end;

function TXMLDocument.XML: String;
begin
  Result := '<?xml version="1.0" encoding="utf-8"?>' + FRootNode.OuterXML;
end;

end.
