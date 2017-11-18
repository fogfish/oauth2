
//
//
const empty = {
   name: null,
   type: null,
   redirect_uri: null
}

const APP_NAME = "@@oauth-app/name";
const APP_TYPE = "@@oauth-app/type";
const APP_REDIRECT_URI = "@@oauth-app/redirect_uri";

//
//
export default (state = empty, action) => {
   switch(action.type)
   {
   case APP_NAME:
      return {...state, name: action.value};
   case APP_TYPE:
      return {...state, type: action.value};
   case APP_REDIRECT_URI:
      return {...state, redirect_uri: action.value};
   default:
      return state
   }
}


//
//
export const onAppName = (e) => ({type: APP_NAME, value: e.target.value})
export const onAppType = (e) => ({type: APP_TYPE, value: e.target.value})
export const onAppRedirectUri = (e) => ({type: APP_REDIRECT_URI, value: e.target.value})


//
//
export const register = () =>
   (dispatch, getState) => (
      fetch('http://localhost:8080/oauth2/client', {
         method: 'POST',
         headers: {
            "Authorization": getState().access_token.bearer
         },
         body: JSON.stringify(getState().app)
      }
      ).then(
         (http) => {
            if (http.ok) {
               return http.json()
            } else {
               return http.json().then((error) => Promise.reject(error))
            }
         }
      ).then(
         (keys) => {
            console.log("= [ ok ]=>", keys)
         }
      ).catch(
         (error) => {
            console.log("= [ er ]=>", error)
         }
      )
   )


