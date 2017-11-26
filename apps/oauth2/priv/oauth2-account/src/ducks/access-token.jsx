
const empty = {}

//
// OAuth2 config
const encodeRequest = (json) => 
   Object.keys(json).map( 
      (key) => [encodeURIComponent(key), encodeURIComponent(json[key])].join('=')
   ).join('&')

const PROVIDER  = "/oauth2/authorize";
const client_id = "oauth2-account";
const response_type = "code";
const state     = "none";
const AUTHORIZE = PROVIDER + '?' + encodeRequest({client_id, response_type, state});

//
// actions
const ACCESS_TOKEN = "@@core/access_token";
const SIGNOUT = "@@core/signout";

//
//
export default (state = empty, action) => {
   switch(action.type)
   {
   case ACCESS_TOKEN:
      return {...state, bearer: action.access_token, ttl: action.ttl};

   case SIGNOUT:
      window.localStorage.removeItem('access_token');
      window.localStorage.removeItem('access_token_ttl');
      window.location = AUTHORIZE;
      return state;

   default:
      return state
   }
}

//
//
export const signin = () => {
   const now = +new Date()
   const access_token = window.localStorage.getItem('access_token')
   const ttl = +window.localStorage.getItem('access_token_ttl')
   if (access_token != null && now < ttl)
      return {type: ACCESS_TOKEN, access_token, ttl}
   else
      return {type: SIGNOUT}
}

//
//
export const signout = () => ({
   type: SIGNOUT
})


