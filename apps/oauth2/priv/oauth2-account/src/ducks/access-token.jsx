
const empty = {}

//
// OAuth2 config
const encodeRequest = (json) => 
   Object.keys(json).map( 
      (key) => [encodeURIComponent(key), encodeURIComponent(json[key])].join('=')
   ).join('&')

const PROVIDER  = "/oauth2/authorize";
const client_id = "oauth2-account";
const response_type = "token";
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
      if (action.access_token)
      {
         return {...state, bearer: action.access_token}
      } else {
         window.location = AUTHORIZE;
         return state;
      };
   case SIGNOUT:
      window.localStorage.removeItem('access_token');
      window.location = AUTHORIZE;
      return state;

   default:
      return state
   }
}

//
//
export const signin = () => ({
   type: ACCESS_TOKEN, 
   access_token: window.localStorage.getItem('access_token')
})

//
//
export const signout = () => ({
   type: SIGNOUT
})


