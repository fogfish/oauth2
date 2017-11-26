
//
//
const empty = {
   oauthApps: [] 
}

const APPEND_OAUTH_APP = "@@acount/append-oauth-app";
const REMOVE_OAUTH_APP = "@@acount/remove-oauth-app";

//
//
export default (state = empty, action) => {
   switch(action.type)
   {
   case APPEND_OAUTH_APP:
      return {...state, oauthApps: state.oauthApps.concat([action.app])};

   case REMOVE_OAUTH_APP:
      return {...state, oauthApps: state.oauthApps.filter(x => x.access != action.app.access)}

   default:
      return state
   }
}

//
export const appendOAuthApp = (app) => ({type: APPEND_OAUTH_APP, app})

export const removeOAuthApp = (app) => ({type: REMOVE_OAUTH_APP, app})