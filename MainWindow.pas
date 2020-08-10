unit MainWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  Vcl.StdCtrls, Vcl.ComCtrls, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, System.Classes, IdSSLOpenSSL, Lang;

const REPO_URL: String = 'https://alexisdelhaie.ovh/dlcenter/chronos-repo/';
const EXECUTABLE: String = 'Chronos.exe';
const ARCHIVE: String = 'chronos.7z';
const ARCHIVE_MD5: String = 'chronos.md5';
const ARCHIVE_SHA1: String = 'chronos.sha';
const CACHE_FOLDER: String = 'cache';

// JSON
const INSTALLED_PATH: String = 'installed_path';
const DESKTOP_SHORTCUT: String = 'desktop_shortcut';
const START_MENU_SHORTCUT: String = 'start_menu_shortcut';
  
type

  TForm1 = class(TForm)
    Image1: TImage;
    versionBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    desktopShortcut: TCheckBox;
    startShortcut: TCheckBox;
    Label3: TLabel;
    PathEdit: TEdit;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    InstallButton: TButton;
    labelCurrentVersion: TLabel;
    FileOpenDialog1: TFileOpenDialog;
    StatusLabel: TLabel;
    LogMemo: TMemo;
    uninstallButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure EnableInstallButton(Sender: TObject);
    procedure InstallButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure uninstallButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FLang: TLang;
    procedure ThreadGetCurrentVersion;
    procedure ThreadGetListOfVersions;
    procedure LockControls(const AValue: Boolean);
    procedure OnHTTPWorkBegin(Sender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure OnHTTPWorking(Sender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure OnHTTPWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
    procedure Unarchive(const APath: String);
    procedure ChangeStatus(const AValue: String);
    procedure DelFilesFromDir(Directory, FileMask: String; DelSubDirs: Boolean);
    procedure CreateShortcut(const targetName: String);
    procedure CreateInformationFile;
    function GetStringFromRepo(const AKey: String; out OValue: String): Boolean;
    function DownloadFileFromRepo(const AVersion: String; AStream: TStream): Boolean;
    function CreateFolderIfNotExist(const APath: String): Boolean;
    function GetTempFile(const AVersion: String): String;
    function MD5(const fileName: String): String;
    function SHA1(const fileName: String): String;
    function CheckHash(const fileName, AVersion: String): Boolean;
    function CreateShellLink(const TargetName, APath: string): Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  JclCompression, ShellAPI, IdHashMessageDigest, json, System.UITypes,
  IdHashSHA, ActiveX, ComObj, ShlObj, IOUtils, JclSysInfo, LangDialog;

{$R *.dfm}

// Create cache folder and returning his path
function TForm1.GetTempFile(const AVersion: String): string;
begin
  if not DirectoryExists(Format('.\%s', [CACHE_FOLDER])) then
  begin
    CreateDir(Format('.\%s', [CACHE_FOLDER]));
  end;
  if not DirectoryExists(Format('.\%s\%s', [CACHE_FOLDER, AVersion])) then
  begin
    CreateDir(Format('.\%s\%s', [CACHE_FOLDER, AVersion]));
  end;
  Result:= Format('.\%s\%s\%s', [CACHE_FOLDER, AVersion, ARCHIVE])
end;

procedure TForm1.EnableInstallButton(Sender: TObject);
begin
  InstallButton.Enabled:= (versionBox.ItemIndex <> -1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLang:= TForm2.GetLanguage(Self);
  Label1.Caption:= FLang.GetTranslation('version');
  labelCurrentVersion.Caption:= Format(FLang.GetTranslation('current_version'), [FLang.GetTranslation('loading')]);
  Label2.Caption:= FLang.GetTranslation('options');
  Label3.Caption:= FLang.GetTranslation('installation_path');
  desktopShortcut.Caption:= FLang.GetTranslation('desktop_shortcut');
  startShortcut.Caption:= FLang.GetTranslation('start_menu_shortcut');
  InstallButton.Caption:= FLang.GetTranslation('install');
  ChangeStatus(FLang.GetTranslation('ready'));
  FileOpenDialog1.Title:= FLang.GetTranslation('choose_install_folder');
  uninstallButton.Caption:= FLang.GetTranslation('uninstall');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FLang.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  save: TJSONObject;
begin
  if FileExists('config.json') then
  begin
    save:= TJSONObject.ParseJSONValue(TFile.ReadAllText('config.json')) as TJSONObject;
    PathEdit.Text:= save.GetValue<String>(INSTALLED_PATH);
    desktopShortcut.Checked:= save.GetValue<Boolean>(DESKTOP_SHORTCUT);
    startShortcut.Checked:= save.GetValue<Boolean>(START_MENU_SHORTCUT);
  end;
  if FileExists(Format('%s\%s', [PathEdit.Text, EXECUTABLE])) then
  begin
    uninstallButton.Visible:= True;
    uninstallButton.Enabled:= True;
    InstallButton.Caption:= FLang.GetTranslation('reinstall');
  end;
  // Starting Thread for fetching all versions
  TThread.CreateAnonymousThread(ThreadGetListOfVersions).Start;
  // Starting thread for fetching current_version
  TThread.CreateAnonymousThread(ThreadGetCurrentVersion).Start;
end;

// Download current stable version and print it into label
procedure TForm1.ThreadGetCurrentVersion;
var
  result_version: string;
  http_success: Boolean;
begin
  http_success:= GetStringFromRepo('current_version', result_version);
  TThread.Synchronize(nil,
  procedure
  begin
    if http_success then
    begin
      labelCurrentVersion.Caption:= Format(FLang.GetTranslation('current_version'), [result_version]);
    end
    else
    begin
      labelCurrentVersion.Visible:= False;
    end;
  end);
end;

// Download list of versions and add it to dropdown
procedure TForm1.ThreadGetListOfVersions;
var
    result_versions: string;
    versions: TStringList;
    http_success: Boolean;
begin

  versions:= TStringList.Create;
  try
    http_success:= GetStringFromRepo('versions', result_versions);
    versions.StrictDelimiter:= True;
    versions.Text:= result_versions;
    TThread.Synchronize(nil,
    procedure
    var v: string;
    begin
      if http_success then
      begin
        if versions.Count > 0 then
        begin
          for v in versions do
          begin
             Form1.versionBox.Items.Add(v);
          end;
          Form1.versionBox.Enabled:= True;
        end;
      end;
    end);
  finally
    versions.Free;
  end;
end;

function TForm1.GetStringFromRepo(const AKey: String; out OValue: String): Boolean;
var
  IdHTTP1: TIdHTTP;
  IdSSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  Result:= False;
  OValue:= '';
  IdHTTP1:= TIdHTTP.Create;
  try
    IdSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      IdHTTP1.IOHandler := IdSSL;
      IdSSL.SSLOptions.Method := sslvTLSv1;
      IdSSL.SSLOptions.Method := sslvTLSv1;
      IdSSL.SSLOptions.Mode := sslmUnassigned;
      try
        OValue:= IdHTTP1.Get(Format('%s%s', [REPO_URL, AKey]));
        Result:= True;
      except
        on E: Exception do begin
          ChangeStatus(Format(FLang.GetTranslation('enable_to_connect_to_repo'), [E.Message]));
        end;
      end;
    finally
      IdSSL.Free;
    end;
  finally
    IdHTTP1.Free;
  end;
end;

procedure TForm1.OpenButtonClick(Sender: TObject);
var
  WS: WideString;
  PWC: PWideChar;
begin
  WS := Format('%s\%s', [PathEdit.Text, EXECUTABLE]);
  PWC := PWideChar(WS);
  ShellExecute(Handle, 'open', PWC, nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.InstallButtonClick(Sender: TObject);
  var
    temp_file_path, version: String;
    downloaded: Boolean;
    s: TStream;
begin
  LockControls(True);
  version:= versionBox.Items[versionBox.ItemIndex];
  if CreateFolderIfNotExist(PathEdit.Text) then
  begin
    temp_file_path:= GetTempFile(version);
    //Save all input to file, maybe useful for uninstall
    CreateInformationFile;
    TThread.CreateAnonymousThread(procedure
    begin
      try
        // Check if archive is cached
        if FileExists(temp_file_path) then
        begin
          ChangeStatus(FLang.GetTranslation('file_existing_in_cache'));
          // If yes, set downloaded flag to true
          downloaded:= True;
        end
        else
        begin
          // Download the archive from the repository
          s := TFileStream.Create(temp_file_path, fmCreate);
          try
            downloaded:= DownloadFileFromRepo(version, s);
          finally
            s.Free;
          end;
        end;

        // Check if download succeed
        if downloaded then
        begin
          TThread.Synchronize(nil,
          procedure
          begin
            ProgressBar1.Style:= TProgressBarStyle.pbstMarquee;
          end);
          try
            // Checking archive integrity
            if CheckHash(temp_file_path, version) then
            begin
              // If software already installed, remove all files
              if FileExists(Format('%s\%s', [PathEdit.Text, EXECUTABLE])) then
              begin
                ChangeStatus(FLang.GetTranslation('clearing_install_folder'));
                DelFilesFromDir(PathEdit.Text, '*.*', True);
              end;
              //Unarchive and create shortcut
              Unarchive(temp_file_path);
              CreateShortcut(Format('%s\%s', [PathEdit.Text, EXECUTABLE]));
              ChangeStatus(FLang.GetTranslation('installation_success'));
              // Change 'install' button to 'open' button
              InstallButton.OnClick:= OpenButtonClick;
              InstallButton.Caption:= FLang.GetTranslation('open');
            end
            else
            begin
              // If cache corrupted, remove archive from cache
              ChangeStatus(FLang.GetTranslation('cleaning_cache'));
              DeleteFile(temp_file_path);
              ChangeStatus(FLang.GetTranslation('install_canceled'));
            end;
          finally
            TThread.Synchronize(nil,
            procedure
            begin
              ProgressBar1.Style:= TProgressBarStyle.pbstNormal;
            end);
          end;
        end;
      finally
        LockControls(False);
      end;
    end).Start;
  end;
end;

function TForm1.DownloadFileFromRepo(const AVersion: String; AStream: TStream): Boolean;
var
  IdHTTP1: TIdHTTP;
  IdSSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  Result:= False;
  IdHTTP1:= TIdHTTP.Create;
  try
    IdSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      IdHTTP1.IOHandler := IdSSL;
      IdHTTP1.OnWorkBegin:= OnHTTPWorkBegin;
      IdHTTP1.OnWork:= OnHTTPWorking;
      IdHTTP1.OnWorkEnd:= OnHTTPWorkEnd;
      IdSSL.SSLOptions.Method := sslvTLSv1;
      IdSSL.SSLOptions.Method := sslvTLSv1;
      IdSSL.SSLOptions.Mode := sslmUnassigned;
      try
        IdHTTP1.Get(Format('%s%s/%s', [REPO_URL, AVersion, ARCHIVE]), AStream);
        Result:= True;
      except
        on E: Exception do begin
          ChangeStatus(Format(FLang.GetTranslation('enable_to_connect_to_repo'), [E.Message]));
          ShowMessage(Format(FLang.GetTranslation('enable_to_connect_to_repo'), [E.Message]));
        end;
      end;
    finally
      IdSSL.Free;
    end;
  finally
    IdHTTP1.Free;
  end;
end;

procedure TForm1.OnHTTPWorkBegin(Sender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  ChangeStatus(Format(FLang.GetTranslation('downloading_of'), [AWorkCountMax / 1000000]));
  TThread.Synchronize(nil,
  procedure
  begin
    ProgressBar1.Position:= 0;
    ProgressBar1.Max:= AWorkCountMax;
  end);
end;

procedure TForm1.OnHTTPWorking(Sender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  TThread.Synchronize(nil,
  procedure
  var
    current_size, max_size: Double;
  begin
    ProgressBar1.Position:= AWorkCount;
    current_size:= AWorkCount / 1000000;
    max_size:= ProgressBar1.Max / 1000000;
    StatusLabel.Caption:= Format(FLang.GetTranslation('downloading'), [current_size, max_size]);
  end);
end;

procedure TForm1.OnHTTPWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
begin
  ChangeStatus(Format(FLang.GetTranslation('downloaded'), [ProgressBar1.Max / 1000000]));
  TThread.Synchronize(nil,
  procedure
  begin
    ProgressBar1.Position:= ProgressBar1.Max;
  end);
end;

procedure TForm1.Unarchive(const APath: String);
var
   archiveclass: TJclDecompressArchiveClass;
   archive: TJclDecompressArchive;
   error: Boolean;
begin
  error:= False;
  archiveclass := GetArchiveFormats.FindDecompressFormat(APath);

  if not Assigned(archiveclass) then
  begin
    ChangeStatus(Format(FLang.GetTranslation('cant_determine_format'), [APath]));
    ShowMessage(Format(FLang.GetTranslation('cant_determine_format'), [APath]));
    error:= True;
  end;
  if not error then
  begin
    archive := archiveclass.Create(APath);
    try
      if not (archive is TJclSevenZipDecompressArchive) then
      begin
        ChangeStatus(FLang.GetTranslation('format_not_supported'));
        ShowMessage(FLang.GetTranslation('format_not_supported'));
        error:= True;
      end;
      if not error then
        begin
        archive.ListFiles;
        ChangeStatus(FLang.GetTranslation('extracting'));
        if archive.ItemCount > 0 then
        begin
          archive.ExtractAll(PathEdit.Text, True);
        end;
        ChangeStatus(FLang.GetTranslation('extracted'));
      end;
    finally
      archive.Free;
    end;
  end;
end;

procedure TForm1.uninstallButtonClick(Sender: TObject);
var
  buttonSelected: Integer;
begin
//DELETE ALL
  buttonSelected := messagedlg(FLang.GetTranslation('uninstall_confirmation'),
                                                    mtConfirmation, mbYesNo, 0);

  // Show the button type selected
  if buttonSelected = mrYes then
  begin
    LockControls(True);
    try
      ChangeStatus(FLang.GetTranslation('uninstallation'));
      DelFilesFromDir(PathEdit.Text, '*.*', True);
      RmDir(PathEdit.Text);
      ChangeStatus(FLang.GetTranslation('uninstallation_succeed'));
    finally
      uninstallButton.Visible:= False;
      LockControls(False);
    end;
  end;
end;

procedure TForm1.LockControls(const AValue: Boolean);
begin
  if (versionBox.ItemIndex <> -1) then
  begin
    InstallButton.Enabled:= not AValue;
  end
  else
  begin
    InstallButton.Enabled:= False;
  end;
  Button1.Enabled:= not AValue;
  versionBox.Enabled:= not AValue;
  desktopShortcut.Enabled:= not AValue;
  startShortcut.Enabled:= not AValue;
  if uninstallButton.Visible then
  begin
    uninstallButton.Enabled:= not AValue;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

  if FileOpenDialog1.Execute then
  begin
    PathEdit.Text:= FileOpenDialog1.FileName;
  end;
end;

function TForm1.CreateFolderIfNotExist(const APath: String): Boolean;
begin
  if not DirectoryExists(APath) then
  begin
    Result:= CreateDir(APath);
  end
  else
  begin
    Result:= True;
  end;
end;

procedure TForm1.ChangeStatus(const AValue: string);
begin
  TThread.Synchronize(nil,
  procedure
  begin
    StatusLabel.Caption:= AValue;
    LogMemo.Lines.Add(AValue);
  end);
end;

procedure TForm1.DelFilesFromDir(Directory, FileMask: string; DelSubDirs: Boolean);
var
  SourceLst: string;
  FOS: TSHFileOpStruct;
begin
  FillChar(FOS, SizeOf(FOS), 0);
  FOS.Wnd := Application.MainForm.Handle;
  FOS.wFunc := FO_DELETE;
  SourceLst := Directory + '\' + FileMask + #0;
  FOS.pFrom := PChar(SourceLst);
  if not DelSubDirs then
    FOS.fFlags := FOS.fFlags OR FOF_FILESONLY;
  // Remove the next line if you want a confirmation dialog box
  FOS.fFlags := FOS.fFlags OR FOF_NOCONFIRMATION;
  // Uncomment the next line for a "silent operation" (no progress box)
  // FOS.fFlags := FOS.fFlags OR FOF_SILENT;
  SHFileOperation(FOS);
end;

function TForm1.MD5(const fileName : string) : string;
var
  idmd5 : TIdHashMessageDigest5;
  fs : TFileStream;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  fs := TFileStream.Create(fileName, fmOpenRead OR fmShareDenyWrite) ;
  try
    Result:= LowerCase(idmd5.HashStreamAsHex(fs));
  finally
    fs.Free;
    idmd5.Free;
  end;
end;

function TForm1.SHA1(const fileName : string) : string;
var
  idsha : TIdHashSHA1;
  fs : TFileStream;
begin
  idsha := TIdHashSHA1.Create;
  fs := TFileStream.Create(fileName, fmOpenRead OR fmShareDenyWrite) ;
  try
    Result:= LowerCase(idsha.HashStreamAsHex(fs));
  finally
    fs.Free;
    idsha.Free;
  end;
end;

function TForm1.CheckHash(const fileName, AVersion: String): Boolean;
var
  sha, md: String;
  shaSuccess, md5Success, md5Match, shaMatch: Boolean;
begin
  ChangeStatus(FLang.GetTranslation('checking_integrity'));
  md5Match:= False;
  // shaMatch ou pas
  shaMatch:= False;
  shaSuccess:= GetStringFromRepo(Format('%s/%s', [AVersion, ARCHIVE_SHA1]), sha);
  md5Success:= GetStringFromRepo(Format('%s/%s', [AVersion, ARCHIVE_MD5]), md);

  if shaSuccess then
  begin
    sha:= sha.Substring(0, 40);
    shaMatch:= (sha = SHA1(fileName));
  end
  else
  begin
    ChangeStatus(FLang.GetTranslation('sha_not_found'));
  end;

  if md5Success then
  begin
    md:= md.Substring(0, 32);
    md5Match:= (md = MD5(fileName));
  end
  else
  begin
    ChangeStatus(FLang.GetTranslation('md5_not_found'));
  end;

  // Both hash are not needed
  Result:= (md5Match OR shaMatch);
  if not Result then
  begin
    ChangeStatus(FLang.GetTranslation('hash_not_matching'));
  end;
end;

procedure TForm1.CreateShortcut(const targetName: String);
begin
  if desktopShortcut.Checked then
  begin
    ChangeStatus(FLang.GetTranslation('create_desktop_shortcut'));
    CreateShellLink(targetName, GetDesktopDirectoryFolder);
  end;
  if startShortcut.Checked then
  begin
    ChangeStatus(FLang.GetTranslation('create_start_menu_shortcut'));
    CreateShellLink(targetName, GetStartmenuFolder);
  end;
end;

// Creating link with Windows API shitty function
function TForm1.CreateShellLink(const TargetName, APath: string): Boolean;
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  PIDL: PItemIDList;
  LinkName: string;
  InFolder: array [0..MAX_PATH-1] of Char;
  WS: WideString;
  PWC: PWideChar;
begin
  CoInitialize(nil);
  try
    Result := False;

    IObject := CreateComObject(CLSID_ShellLink);
    ISLink := IObject as IShellLink;
    IPFile := IObject as IPersistFile;

    with ISLink do
    begin
      WS := FLang.GetTranslation('shortcut_description');
      PWC := PWideChar(WS);
      SetDescription(PWC);
      SetPath(PChar(TargetName));
      SetWorkingDirectory(PChar(ExtractFilePath(TargetName)));
    end;

    SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
    SHGetPathFromIDList(PIDL, InFolder) ;

    LinkName := IncludeTrailingPathDelimiter(APath);
    LinkName := LinkName + ExtractFileName(TargetName) + '.lnk';

    if not FileExists(LinkName) then
    begin
      if IPFile.Save(PWideChar(LinkName), False) = S_OK then
      begin
        Result := True;
      end;
    end
  finally
    CoUninitialize;
  end;
end;

procedure TForm1.CreateInformationFile;
var
  save: TJSONObject;
  myFile : TextFile;
begin
  save:= TJSONObject.Create;
  try
    save.AddPair(INSTALLED_PATH, PathEdit.Text);
    save.AddPair(DESKTOP_SHORTCUT, TJSONBool.Create(desktopShortcut.Checked));
    save.AddPair(START_MENU_SHORTCUT, TJSONBool.Create(startShortcut.Checked));
    AssignFile(myFile, 'config.json');
    try
      ReWrite(myFile);
      WriteLn(myFile, save.ToJSON);
    finally
      CloseFile(myFile);
    end;
  finally
    save.Free;
  end;
end;

end.
