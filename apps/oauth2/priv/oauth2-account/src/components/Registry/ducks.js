import { APPLICATION } from '../NewApp/ducks'

const REVOKE = "@@registry/revoke"

//
//
export const lookup = () =>
  async (dispatch, getState) => {
    const apps = await fetch('/oauth2/client', {
      method: 'GET',
      headers: {
        "Authorization": getState().oauth2.token
      }
    }).then(jsonify)

    console.log("=======")
    console.log(apps)
  }

const jsonify = (http) => {
   if (http.ok) {
      return http.json()
   } else {
      return http.json().then(error => Promise.reject(error))
   }
}

//
//
export const revoke = (access) =>
  async (dispatch, getState) => {
    const app = await fetch(`/oauth2/client/${access}`, {
      method: 'DELETE',
      headers: {
        "Authorization": getState().oauth2.token
      }
    }).then(jsonify)

    dispatch({type: REVOKE, access})
  }


//
//
const empty = {
  apps: []
}

export default (state = empty, action) => {
  switch (action.type) {
    case APPLICATION:
      const name = action.name
      const access = action.access
      const redirect_uri = action.redirect_uri
      const security = action.security
      return { ...state, apps: state.apps.concat([{name, access, redirect_uri, security}])}
    case REVOKE:
      return { ...state, apps: state.apps.filter(x => x.access != action.access)}
    default:
      return { ...state }
  }
}   