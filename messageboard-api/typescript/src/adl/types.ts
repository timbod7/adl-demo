/* @generated from adl module types */

import * as ADL from './runtime/adl';

export interface Empty {
}

export function makeEmpty(
  _input: {
  }
): Empty {
  return {
  };
}

const Empty_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[]}},"name":"Empty","version":{"kind":"nothing"}}};

export const snEmpty: ADL.ScopedName = {moduleName:"types", name:"Empty"};

export function texprEmpty(): ADL.ATypeExpr<Empty> {
  return {value : {typeRef : {kind: "reference", value : snEmpty}, parameters : []}};
}

export type Jwt = string;

const Jwt_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Jwt","version":{"kind":"nothing"}}};

export const snJwt: ADL.ScopedName = {moduleName:"types", name:"Jwt"};

export function texprJwt(): ADL.ATypeExpr<Jwt> {
  return {value : {typeRef : {kind: "reference", value : snJwt}, parameters : []}};
}

export type Email = string;

const Email_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Email","version":{"kind":"nothing"}}};

export const snEmail: ADL.ScopedName = {moduleName:"types", name:"Email"};

export function texprEmail(): ADL.ATypeExpr<Email> {
  return {value : {typeRef : {kind: "reference", value : snEmail}, parameters : []}};
}

export type Password = string;

const Password_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Password","version":{"kind":"nothing"}}};

export const snPassword: ADL.ScopedName = {moduleName:"types", name:"Password"};

export function texprPassword(): ADL.ATypeExpr<Password> {
  return {value : {typeRef : {kind: "reference", value : snPassword}, parameters : []}};
}

export type UserId = string;

const UserId_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"UserId","version":{"kind":"nothing"}}};

export const snUserId: ADL.ScopedName = {moduleName:"types", name:"UserId"};

export function texprUserId(): ADL.ATypeExpr<UserId> {
  return {value : {typeRef : {kind: "reference", value : snUserId}, parameters : []}};
}

export type TimeStamp = string;

const TimeStamp_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"TimeStamp","version":{"kind":"nothing"}}};

export const snTimeStamp: ADL.ScopedName = {moduleName:"types", name:"TimeStamp"};

export function texprTimeStamp(): ADL.ATypeExpr<TimeStamp> {
  return {value : {typeRef : {kind: "reference", value : snTimeStamp}, parameters : []}};
}

export interface HttpPost<I, O> {
  path: string;
  security: HttpSecurity;
  reqType: ADL.ATypeExpr<I>;
  respType: ADL.ATypeExpr<O>;
}

const HttpPost_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["I","O"],"fields":[{"annotations":[],"serializedName":"path","default":{"kind":"nothing"},"name":"path","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"security","default":{"kind":"nothing"},"name":"security","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"HttpSecurity"}},"parameters":[]}},{"annotations":[],"serializedName":"reqType","default":{"kind":"just","value":null},"name":"reqType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"I"},"parameters":[]}]}},{"annotations":[],"serializedName":"respType","default":{"kind":"just","value":null},"name":"respType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"O"},"parameters":[]}]}}]}},"name":"HttpPost","version":{"kind":"nothing"}}};

export const snHttpPost: ADL.ScopedName = {moduleName:"types", name:"HttpPost"};

export function texprHttpPost<I, O>(texprI : ADL.ATypeExpr<I>, texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<HttpPost<I, O>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "types",name : "HttpPost"}}, parameters : [texprI.value, texprO.value]}};
}

export enum HttpSecurity {
  public,
  token,
  adminToken,
}

const HttpSecurity_AST : ADL.ScopedDecl =
  {"moduleName":"types","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"public","default":{"kind":"nothing"},"name":"public","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"token","default":{"kind":"nothing"},"name":"token","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"adminToken","default":{"kind":"nothing"},"name":"adminToken","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"HttpSecurity","version":{"kind":"nothing"}}};

export const snHttpSecurity: ADL.ScopedName = {moduleName:"types", name:"HttpSecurity"};

export function texprHttpSecurity(): ADL.ATypeExpr<HttpSecurity> {
  return {value : {typeRef : {kind: "reference", value : snHttpSecurity}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "types.Empty" : Empty_AST,
  "types.Jwt" : Jwt_AST,
  "types.Email" : Email_AST,
  "types.Password" : Password_AST,
  "types.UserId" : UserId_AST,
  "types.TimeStamp" : TimeStamp_AST,
  "types.HttpPost" : HttpPost_AST,
  "types.HttpSecurity" : HttpSecurity_AST
};
