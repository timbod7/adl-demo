/* @generated from adl module api */

import * as ADL from './runtime/adl';
import * as types from './types';

export interface Api {
  ping: types.HttpPost<types.Empty, types.Empty>;
  login: types.HttpPost<LoginReq, types.Jwt>;
  newMessage: types.HttpPost<NewMessageReq, types.Empty>;
  recentMessages: types.HttpPost<RecentMessagesReq, Message[]>;
  createUser: types.HttpPost<CreateUserReq, CreateUserResp>;
}

export function makeApi(
  input: {
    ping?: types.HttpPost<types.Empty, types.Empty>,
    login?: types.HttpPost<LoginReq, types.Jwt>,
    newMessage?: types.HttpPost<NewMessageReq, types.Empty>,
    recentMessages?: types.HttpPost<RecentMessagesReq, Message[]>,
    createUser?: types.HttpPost<CreateUserReq, CreateUserResp>,
  }
): Api {
  return {
    ping: input.ping === undefined ? {path : "/ping", security : 0, reqType : types.texprEmpty(), respType : types.texprEmpty()} : input.ping,
    login: input.login === undefined ? {path : "/login", security : 0, reqType : texprLoginReq(), respType : types.texprJwt()} : input.login,
    newMessage: input.newMessage === undefined ? {path : "/new-message", security : 1, reqType : texprNewMessageReq(), respType : types.texprEmpty()} : input.newMessage,
    recentMessages: input.recentMessages === undefined ? {path : "/recent-messages", security : 1, reqType : texprRecentMessagesReq(), respType : ADL.texprVector(texprMessage())} : input.recentMessages,
    createUser: input.createUser === undefined ? {path : "/create-user", security : 2, reqType : texprCreateUserReq(), respType : texprCreateUserResp()} : input.createUser,
  };
}

const Api_AST : ADL.ScopedDecl =
  {"moduleName":"api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"ping","default":{"kind":"just","value":{"path":"/ping","security":"public"}},"name":"ping","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"Empty"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"Empty"}},"parameters":[]}]}},{"annotations":[],"serializedName":"login","default":{"kind":"just","value":{"path":"/login","security":"public"}},"name":"login","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"api","name":"LoginReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"Jwt"}},"parameters":[]}]}},{"annotations":[],"serializedName":"newMessage","default":{"kind":"just","value":{"path":"/new-message","security":"token"}},"name":"newMessage","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"api","name":"NewMessageReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"Empty"}},"parameters":[]}]}},{"annotations":[],"serializedName":"recentMessages","default":{"kind":"just","value":{"path":"/recent-messages","security":"token"}},"name":"recentMessages","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"api","name":"RecentMessagesReq"}},"parameters":[]},{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"api","name":"Message"}},"parameters":[]}]}]}},{"annotations":[],"serializedName":"createUser","default":{"kind":"just","value":{"path":"/create-user","security":"adminToken"}},"name":"createUser","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"api","name":"CreateUserReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"api","name":"CreateUserResp"}},"parameters":[]}]}}]}},"name":"Api","version":{"kind":"nothing"}}};

export const snApi: ADL.ScopedName = {moduleName:"api", name:"Api"};

export function texprApi(): ADL.ATypeExpr<Api> {
  return {value : {typeRef : {kind: "reference", value : snApi}, parameters : []}};
}

export interface LoginReq {
  email: types.Email;
  password: string;
}

export function makeLoginReq(
  input: {
    email: types.Email,
    password: string,
  }
): LoginReq {
  return {
    email: input.email,
    password: input.password,
  };
}

const LoginReq_AST : ADL.ScopedDecl =
  {"moduleName":"api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"email","default":{"kind":"nothing"},"name":"email","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"Email"}},"parameters":[]}},{"annotations":[],"serializedName":"password","default":{"kind":"nothing"},"name":"password","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"LoginReq","version":{"kind":"nothing"}}};

export const snLoginReq: ADL.ScopedName = {moduleName:"api", name:"LoginReq"};

export function texprLoginReq(): ADL.ATypeExpr<LoginReq> {
  return {value : {typeRef : {kind: "reference", value : snLoginReq}, parameters : []}};
}

export interface NewMessageReq {
  body: string;
}

export function makeNewMessageReq(
  input: {
    body: string,
  }
): NewMessageReq {
  return {
    body: input.body,
  };
}

const NewMessageReq_AST : ADL.ScopedDecl =
  {"moduleName":"api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"body","default":{"kind":"nothing"},"name":"body","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"NewMessageReq","version":{"kind":"nothing"}}};

export const snNewMessageReq: ADL.ScopedName = {moduleName:"api", name:"NewMessageReq"};

export function texprNewMessageReq(): ADL.ATypeExpr<NewMessageReq> {
  return {value : {typeRef : {kind: "reference", value : snNewMessageReq}, parameters : []}};
}

export interface RecentMessagesReq {
  maxMessages: number;
}

export function makeRecentMessagesReq(
  input: {
    maxMessages: number,
  }
): RecentMessagesReq {
  return {
    maxMessages: input.maxMessages,
  };
}

const RecentMessagesReq_AST : ADL.ScopedDecl =
  {"moduleName":"api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"maxMessages","default":{"kind":"nothing"},"name":"maxMessages","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}}]}},"name":"RecentMessagesReq","version":{"kind":"nothing"}}};

export const snRecentMessagesReq: ADL.ScopedName = {moduleName:"api", name:"RecentMessagesReq"};

export function texprRecentMessagesReq(): ADL.ATypeExpr<RecentMessagesReq> {
  return {value : {typeRef : {kind: "reference", value : snRecentMessagesReq}, parameters : []}};
}

export interface CreateUserReq {
  email: types.Email;
  password: types.Password;
  isAdmin: boolean;
}

export function makeCreateUserReq(
  input: {
    email: types.Email,
    password: types.Password,
    isAdmin: boolean,
  }
): CreateUserReq {
  return {
    email: input.email,
    password: input.password,
    isAdmin: input.isAdmin,
  };
}

const CreateUserReq_AST : ADL.ScopedDecl =
  {"moduleName":"api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"email","default":{"kind":"nothing"},"name":"email","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"Email"}},"parameters":[]}},{"annotations":[],"serializedName":"password","default":{"kind":"nothing"},"name":"password","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"Password"}},"parameters":[]}},{"annotations":[],"serializedName":"isAdmin","default":{"kind":"nothing"},"name":"isAdmin","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"CreateUserReq","version":{"kind":"nothing"}}};

export const snCreateUserReq: ADL.ScopedName = {moduleName:"api", name:"CreateUserReq"};

export function texprCreateUserReq(): ADL.ATypeExpr<CreateUserReq> {
  return {value : {typeRef : {kind: "reference", value : snCreateUserReq}, parameters : []}};
}

export enum CreateUserResp {
  success,
  duplicateEmail,
}

const CreateUserResp_AST : ADL.ScopedDecl =
  {"moduleName":"api","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"success","default":{"kind":"nothing"},"name":"success","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"duplicateEmail","default":{"kind":"nothing"},"name":"duplicateEmail","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"CreateUserResp","version":{"kind":"nothing"}}};

export const snCreateUserResp: ADL.ScopedName = {moduleName:"api", name:"CreateUserResp"};

export function texprCreateUserResp(): ADL.ATypeExpr<CreateUserResp> {
  return {value : {typeRef : {kind: "reference", value : snCreateUserResp}, parameters : []}};
}

export interface Message {
  id: string;
  postedBy: types.Email;
  postedAt: types.TimeStamp;
  body: string;
}

export function makeMessage(
  input: {
    id: string,
    postedBy: types.Email,
    postedAt: types.TimeStamp,
    body: string,
  }
): Message {
  return {
    id: input.id,
    postedBy: input.postedBy,
    postedAt: input.postedAt,
    body: input.body,
  };
}

const Message_AST : ADL.ScopedDecl =
  {"moduleName":"api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"id","default":{"kind":"nothing"},"name":"id","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"postedBy","default":{"kind":"nothing"},"name":"postedBy","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"Email"}},"parameters":[]}},{"annotations":[],"serializedName":"postedAt","default":{"kind":"nothing"},"name":"postedAt","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"types","name":"TimeStamp"}},"parameters":[]}},{"annotations":[],"serializedName":"body","default":{"kind":"nothing"},"name":"body","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"Message","version":{"kind":"nothing"}}};

export const snMessage: ADL.ScopedName = {moduleName:"api", name:"Message"};

export function texprMessage(): ADL.ATypeExpr<Message> {
  return {value : {typeRef : {kind: "reference", value : snMessage}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "api.Api" : Api_AST,
  "api.LoginReq" : LoginReq_AST,
  "api.NewMessageReq" : NewMessageReq_AST,
  "api.RecentMessagesReq" : RecentMessagesReq_AST,
  "api.CreateUserReq" : CreateUserReq_AST,
  "api.CreateUserResp" : CreateUserResp_AST,
  "api.Message" : Message_AST
};
