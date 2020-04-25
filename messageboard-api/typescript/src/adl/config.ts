/* @generated from adl module config */

import * as ADL from './runtime/adl';

export interface ServerConfig {
  port: number;
  jwtSecret: string;
}

export function makeServerConfig(
  input: {
    port: number,
    jwtSecret: string,
  }
): ServerConfig {
  return {
    port: input.port,
    jwtSecret: input.jwtSecret,
  };
}

const ServerConfig_AST : ADL.ScopedDecl =
  {"moduleName":"config","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"port","default":{"kind":"nothing"},"name":"port","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"jwtSecret","default":{"kind":"nothing"},"name":"jwtSecret","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"ServerConfig","version":{"kind":"nothing"}}};

export const snServerConfig: ADL.ScopedName = {moduleName:"config", name:"ServerConfig"};

export function texprServerConfig(): ADL.ATypeExpr<ServerConfig> {
  return {value : {typeRef : {kind: "reference", value : snServerConfig}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "config.ServerConfig" : ServerConfig_AST
};
