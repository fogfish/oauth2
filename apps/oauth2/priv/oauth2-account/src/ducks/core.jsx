
import {createOAuthApp} from './oauth-app'

//
// default empty status
const empty = {
   isOAuthApps: true,
   isOAuthAppRegister: false,
   isOAuthAppCredentials: false
}

//
//
const APPSVIEW = "@@core/appsview";
const REGISTER = "@@core/register";
const CREDENTIALS = "@@core/credentials";


//
//
export default (state = empty, action) => {
   switch(action.type)
   {
   case APPSVIEW:
      return {...state, isOAuthApps: true, isOAuthAppRegister: false, isOAuthAppCredentials: false};

   case REGISTER:
      return {...state, isOAuthApps: false, isOAuthAppRegister: true, isOAuthAppCredentials: false};

   case CREDENTIALS:
      return {...state, isOAuthApps: false, isOAuthAppRegister: false, isOAuthAppCredentials: true};

   default:
      return state
   }
}


export const appsview = () => ({type: APPSVIEW})

export const showRegisterNewApp = () => 
   (dispatch) => {
      dispatch(createOAuthApp())
      dispatch({type: REGISTER})
   }

export const showSecretKeys = () => ({type: CREDENTIALS})
