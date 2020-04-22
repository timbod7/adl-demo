import { HttpFetch } from "./http";
import * as ADL from "../adl/runtime/adl";
import * as API from "../adl/api";
import { AuthReqFn, ReqFn, ServiceBase } from "./service-base";
import { Jwt, Empty } from "../adl/types";

const api = API.makeApi({});

// Implements typed access to the authenticated API endpoints
export class Service extends ServiceBase {
  constructor(
    http: HttpFetch,
    baseUrl: string,
    resolver: ADL.DeclResolver,
  ) {
    super(http, baseUrl, resolver);
  }

  login: ReqFn<API.LoginReq, Jwt> = this.mkPostFn(api.login);
  ping: ReqFn<Empty, Empty> = this.mkPostFn(api.ping);
  newMessage: AuthReqFn<API.NewMessageReq,Empty> = this.mkAuthPostFn(api.newMessage);
  recentMessages: AuthReqFn<API.RecentMessagesReq,API.Message[]> = this.mkAuthPostFn(api.recentMessages);
  createUser: AuthReqFn<API.CreateUserReq,API.CreateUserResp> = this.mkAuthPostFn(api.createUser);
};
