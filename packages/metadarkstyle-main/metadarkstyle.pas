{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MetaDarkStyle;

{$warn 5023 off : no warning about unused units}
interface

uses
  uMetaDarkStyle, uDarkStyleParams, uDarkStyleSchemesAdditional, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MetaDarkStyle', @Register);
end.
