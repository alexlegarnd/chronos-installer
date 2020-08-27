unit Lang;

interface

uses
  System.Classes, System.Generics.Collections;

type

TLang = class
  private
    available: TStringList;
    langs: TDictionary<string, TDictionary<string, string>>;
    selected_lang: string;

    function GetAvailableLanguages: TStringList;
    procedure SetSelectedLanguage(const ALang: String);
  public
    constructor Create;
    destructor Destroy; override;
    function GetTranslation(const AKey: string): string;
    function GetFullNameLanguage(const AKey: String): String;
    property AvailableLanguages: TStringList read GetAvailableLanguages;
    property SelectedLanguage: String read selected_lang write SetSelectedLanguage;
end;

implementation

uses
  json, System.Types, System.SysUtils;

{ TLang }

constructor TLang.Create;
var
  ResStream: TResourceStream;
  st: TStringStream;
  raw: string;
  doc, ls, l: TJSONObject;
  lang: TDictionary<string, string>;
  i, y: Integer;
begin
  available:= TStringList.Create;
  st:= TStringStream.Create;
  langs:= TDictionary<string, TDictionary<string, string>>.Create;
  ResStream := TResourceStream.Create(hInstance, 'DATA_LANG', RT_RCDATA);
  try
    ResStream.SaveToStream(st);
    raw:= st.DataString;
    doc := TJSONObject.ParseJSONValue(raw) as TJSONObject;
    try
      ls:= doc.GetValue<TJSONObject>('languages');
      for i := 0 to Pred(ls.Count) do
      begin
        lang:= TDictionary<string, string>.Create;
        l:= ls.Pairs[i].JsonValue as TJSONObject;
        for y := 0 to Pred(l.Count) do
        begin
          lang.Add(l.Pairs[y].JsonString.Value, l.Pairs[y].JsonValue.Value);
        end;
        langs.Add(ls.Pairs[i].JsonString.Value, lang);
        available.Add(ls.Pairs[i].JsonString.Value);
      end;
      selected_lang:= available[0];
    finally
      doc.Free;  
    end;
  finally
    ResStream.Free;
    st.Free;
  end;
end;

destructor TLang.Destroy;
begin
  available.Free;
  langs.Free;
  inherited;
end;

function TLang.GetAvailableLanguages;
begin
  Result:= available;
end;

function TLang.GetTranslation(const AKey: string): string;
var
  lang: TDictionary<string, string>;
begin
  try
  lang:= langs[selected_lang];
  Result:= lang[AKey];
  except
    on E: EListError do
    begin
      Result:= AKey;
    end;
  end;
end;

procedure TLang.SetSelectedLanguage(const ALang: string);
begin
  selected_lang:= ALang;
end;

function TLang.GetFullNameLanguage(const AKey: String): String;
var
  lang: TDictionary<string, string>;
begin
  try
  lang:= langs[AKey];
  Result:= lang['language'];
  except
    on E: EListError do
    begin
      Result:= AKey;
    end;
  end;
end;

end.
