{GUI demo for crcmodel/crcm_cat, (c) 2008 W.Ehrhardt}

program tcrc16;

uses
  Forms,
  tcrc16u in 'tcrc16u.pas' {Form1},
  crcm_cat in 'crcm_cat.pas',
  crcmodel in 'crcmodel.pas',
  Mem_Util in 'mem_util.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
