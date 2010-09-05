{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Nov 29, 2008.
*******************************************************************************
Build a better and more sustainable version of defines.
Todo: Delphi 2010 and XE
******************************************************************************}

// First define the proper version define
// Found the defines at
// http://delphi.wikia.com/wiki/Borland_Compiler_Conditional_Defines

{$UNDEF DOTNET}

{$UNDEF DELPHI1}  {$IFDEF VER80}  {$DEFINE DELPHI1} {$ENDIF}
{$UNDEF DELPHI2}  {$IFDEF VER90}  {$DEFINE DELPHI2} {$ENDIF}
{$UNDEF BCB1}     {$IFDEF VER93}  {$DEFINE BCB1}    {$ENDIF}
{$UNDEF DELPHI3}  {$IFDEF VER100} {$DEFINE DELPHI3} {$ENDIF}
{$UNDEF BCB3}     {$IFDEF VER110} {$DEFINE BCB3}    {$ENDIF}
{$UNDEF DELPHI4}  {$IFDEF VER120} {$DEFINE DELPHI4} {$ENDIF}
{$UNDEF BCB4}     {$IFDEF VER125} {$DEFINE BCB4}    {$ENDIF}
// VER130 is shared between Delphi 5 and BCB 5
{$UNDEF DELPHI5}
{$UNDEF BCB5}
{$IFDEF VER130} {$IFDEF BCB} {$DEFINE BCB5} {$ELSE} {$DEFINE DELPHI5} {$ENDIF} {$ENDIF}
// VER140 is shared between Delphi 6, BCB 6 and Kylix 1, 2 and 3.
{$UNDEF DELPHI6}
{$UNDEF BCB6}
  {$IFDEF VER140}
    {$IFNDEF LINUX}
      {$IFDEF BCB}
        {$DEFINE BCB6}
      {$ELSE}
        {$DEFINE DELPHI6}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$UNDEF DELPHI7}  {$IFDEF VER150} {$DEFINE DELPHI7} {$ENDIF}
// Note: VER160 is used by C#Builder as well
{$UNDEF DELPHI8}  {$IFDEF VER160} {$DEFINE DELPHIDOTNET8} {$ENDIF}
{$UNDEF DELPHI9}  {$IFDEF VER170} {$DEFINE DELPHI9} {$ENDIF}
// VER180 is used by Developer Studion 2006 (11) and Delphi 2007.
// VER185 is used by Delphi 2007 only.
{$UNDEF DELPHI10}
{$UNDEF BCB10}
{$UNDEF DELPHI11}
  {$IFDEF VER180}
    {$IFNDEF VER185}
      {$IFDEF BCB}
        {$DEFINE BCB10}
      {$ELSE}
        {$DEFINE DELPHI10}
      {$ENDIF}
    {$ELSE}
      {$DEFINE DELPHI11}
    {$ENDIF}
  {$ENDIF}
// Delphi 2007.NET
{$IFDEF VER190} {$DEFINE DELPHIDOTNET11} {$ENDIF}
// VER200 is used by Delphi 2009 as well as CCB 2009.
{$UNDEF DELPHI12}
{$UNDEF BCB12}
  {$IFDEF VER200}
    {$IFDEF BCB}
      {$DEFINE BCB12}
    {$ELSE}
      {$DEFINE DELPHI12}
    {$ENDIF}
  {$ENDIF}


{$IFDEF DELPHI1}        {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP} {$ENDIF}
{$IFDEF DELPHI2}        {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP}
                        {$DEFINE DELPHI2UP} {$ENDIF}
{$IFDEF DELPHI3}        {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP}
                        {$DEFINE DELPHI2UP}
                        {$DEFINE DELPHI3UP} {$ENDIF}
{$IFDEF DELPHI4}        {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP}
                        {$DEFINE DELPHI2UP}
                        {$DEFINE DELPHI3UP}
                        {$DEFINE DELPHI4UP} {$ENDIF}
{$IFDEF DELPHI5}        {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP}
                        {$DEFINE DELPHI2UP}
                        {$DEFINE DELPHI3UP}
                        {$DEFINE DELPHI4UP}
                        {$DEFINE DELPHI5UP} {$ENDIF}
{$IFDEF DELPHI6}        {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP}
                        {$DEFINE DELPHI2UP}
                        {$DEFINE DELPHI3UP}
                        {$DEFINE DELPHI4UP}
                        {$DEFINE DELPHI5UP}
                        {$DEFINE DELPHI6UP} {$ENDIF}
{$IFDEF DELPHI7}        {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP}
                        {$DEFINE DELPHI2UP}
                        {$DEFINE DELPHI3UP}
                        {$DEFINE DELPHI4UP}
                        {$DEFINE DELPHI5UP}
                        {$DEFINE DELPHI6UP}
                        {$DEFINE DELPHI7UP} {$ENDIF}
{$IFDEF DELPHI9}        {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP}
                        {$DEFINE DELPHI2UP}
                        {$DEFINE DELPHI3UP}
                        {$DEFINE DELPHI4UP}
                        {$DEFINE DELPHI5UP}
                        {$DEFINE DELPHI6UP}
                        {$DEFINE DELPHI7UP}
                        {$DEFINE DELPHI9UP} {$ENDIF}
{$IFDEF DELPHI10}       {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP}
                        {$DEFINE DELPHI2UP}
                        {$DEFINE DELPHI3UP}
                        {$DEFINE DELPHI4UP}
                        {$DEFINE DELPHI5UP}
                        {$DEFINE DELPHI6UP}
                        {$DEFINE DELPHI7UP}
                        {$DEFINE DELPHI9UP}
                        {$DEFINE DELPHI10UP} {$ENDIF}
{$IFDEF DELPHI11}       {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP}
                        {$DEFINE DELPHI2UP}
                        {$DEFINE DELPHI3UP}
                        {$DEFINE DELPHI4UP}
                        {$DEFINE DELPHI5UP}
                        {$DEFINE DELPHI6UP}
                        {$DEFINE DELPHI7UP}
                        {$DEFINE DELPHI9UP}
                        {$DEFINE DELPHI10UP}
                        {$DEFINE DELPHI11UP} {$ENDIF}
{$IFDEF DELPHI12}       {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP}
                        {$DEFINE DELPHI2UP}
                        {$DEFINE DELPHI3UP}
                        {$DEFINE DELPHI4UP}
                        {$DEFINE DELPHI5UP}
                        {$DEFINE DELPHI6UP}
                        {$DEFINE DELPHI7UP}
                        {$DEFINE DELPHI9UP}
                        {$DEFINE DELPHI10UP}
                        {$DEFINE DELPHI11UP}
                        {$DEFINE DELPHI12UP} {$ENDIF}

{$IFDEF DELPHIDOTNET8}  {$DEFINE DOTNET}
                        {$DEFINE DELPHIDOTNET8UP} {$ENDIF}

{$IFDEF DELPHIDOTNET11} {$DEFINE DOTNET}
                        {$DEFINE DELPHIDOTNET8UP}
                        {$DEFINE DELPHIDOTNET11UP} {$ENDIF}

{$IFDEF LINUX}          {$DEFINE DELPHI}
                        {$DEFINE DELPHI1UP}
                        {$DEFINE DELPHI2UP}
                        {$DEFINE DELPHI3UP}
                        {$DEFINE DELPHI4UP}
                        {$DEFINE DELPHI5UP}
                        {$DEFINE DELPHI6UP} {$ENDIF}
{$IFDEF BCB1}           {$DEFINE BCB}
                        {$DEFINE BCB1UP}    {$ENDIF}
{$IFDEF BCB3}           {$DEFINE BCB}
                        {$DEFINE BCB1UP}
                        {$DEFINE BCB3UP}    {$ENDIF}
{$IFDEF BCB4}           {$DEFINE BCB}
                        {$DEFINE BCB1UP}
                        {$DEFINE BCB3UP}
                        {$DEFINE BCB4UP}    {$ENDIF}

{$IFNDEF DELPHI}{$IFNDEF BCB}{$IFNDEF DOTNET}
  Unsupported compiler version.
{$ENDIF}{$ENDIF}{$ENDIF}

