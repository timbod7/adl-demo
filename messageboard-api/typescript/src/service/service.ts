import { HttpFetch } from "./http";
import * as ADL from "../adl/runtime/adl";
import * as API from "../adl/api";
import { ReqFn, ServiceBase } from "./service-base";
import { Jwt, Empty } from "../adl/types";

const api = API.makeApi({});

// Implements typed access to the public API endpoints
export class PublicService extends ServiceBase {
  constructor(
    http: HttpFetch,
    baseUrl: string,
    resolver: ADL.DeclResolver,
  ) {
    super(http, baseUrl, resolver, undefined);
  }

  login: ReqFn<API.LoginReq, Jwt> = this.mkPostFn(api.login);
  ping: ReqFn<Empty, Empty> = this.mkPostFn(api.ping);
}

// Implements typed access to the authenticated API endpoints
export class Service extends ServiceBase {
  constructor(
    http: HttpFetch,
    baseUrl: string,
    resolver: ADL.DeclResolver,
    accessToken: string,
  ) {
    super(http, baseUrl, resolver, accessToken);
  }

  newMessage: ReqFn<API.NewMessageReq,Empty> = this.mkPostFn(api.newMessage);
  recentMessages: ReqFn<API.RecentMessagesReq,API.Message[]> = this.mkPostFn(api.recentMessages);
  createUser: ReqFn<API.CreateUserReq,API.CreateUserResp> = this.mkPostFn(api.createUser);
};
