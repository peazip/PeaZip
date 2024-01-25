{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit metadarkstyledsgn226;

{$warn 5023 off : no warning about unused units}
interface

uses
  registerMetaDarkStyleDSGN, MetaDarkStyleDSGNOptionsFrame, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registerMetaDarkStyleDSGN', @registerMetaDarkStyleDSGN.Register
    );
end;

initialization
  RegisterPackage('metadarkstyledsgn226', @Register);
end.
