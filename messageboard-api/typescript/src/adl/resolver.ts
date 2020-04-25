/* @generated from adl */
import { declResolver, ScopedDecl } from "./runtime/adl";
import { _AST_MAP as api } from "./api";
import { _AST_MAP as config } from "./config";
import { _AST_MAP as sys_annotations } from "./sys/annotations";
import { _AST_MAP as types } from "./types";

export const ADL: { [key: string]: ScopedDecl } = {
  ...api,
  ...config,
  ...sys_annotations,
  ...types,
};

export const RESOLVER = declResolver(ADL);
