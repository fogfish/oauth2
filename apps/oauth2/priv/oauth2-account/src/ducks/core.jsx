
import {createOAuthApp} from './oauth-app'

//
// default empty status
const empty = {
   isOAuthApps: true,
   isOAuthAppRegister: false,
   isOAuthAppCredentials: false,

   isLoading: false,
   failure: ""
}

//
//
const APPSVIEW = "@@core/appsview";
const REGISTER = "@@core/register";
const CREDENTIALS = "@@core/credentials";
const SHOW_LOADING = "@@core/show-loading";
const HIDE_LOADING = "@@core/hide-loading";
const SHOW_FAILURE = "@@core/show-failure";

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

   case SHOW_LOADING:
      return {...state, isLoading: true};

   case HIDE_LOADING:
      return {...state, isLoading: false};

   case SHOW_FAILURE:
      return {...state, failure: action.error}

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


export const showLoading = () => ({type: SHOW_LOADING})
export const hideLoading = () => ({type: HIDE_LOADING})

export const showFailure = (text) => 
   (dispatch) => {
      dispatch({type: SHOW_FAILURE, error: text})
      setTimeout((x) => dispatch({type: SHOW_FAILURE, error: ''}), 3000)
   }




