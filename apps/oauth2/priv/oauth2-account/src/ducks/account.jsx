
//
//
const empty = {
   oauthApps: [] 
}

const ADD_OAUTH_APP = "@@acount/add-oauth-app";

//
//
export default (state = empty, action) => {
   switch(action.type)
   {
   case ADD_OAUTH_APP:
      return {...state, oauthApps: state.oauthApps.concat([action.app])};
   default:
      return state
   }
}

//
export const appendOAuthApp = (app) => ({type: ADD_OAUTH_APP, app})

