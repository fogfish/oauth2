//
//
const SIGNIN   = '@oauth2/signin'
const EXCHANGE = '@oauth2/exchange'
const ERROR    = '@oauth2/error'

//
// encode / decode utilities
const keyval = (key, val) => (key === "" ? val : decodeURIComponent(val))
const decode = text => (text ? JSON.parse('{"' + text.replace(/&/g, '","').replace(/=/g,'":"') + '"}', keyval) : {})
const encode = json => (Object.keys(json).map((key) => ([encodeURIComponent(key), encodeURIComponent(json[key])].join('='))).join('&'))

//
//
const OAUTH2_AUTHORIZE = process.env.REACT_APP_OAUTH2_AUTHORIZE
const OAUTH2_TOKEN = process.env.REACT_APP_OAUTH2_TOKEN
const OAUTH2_CLIENT_ID = process.env.REACT_APP_OAUTH2_CLIENT_ID
const OAUTH2_FLOW_TYPE = process.env.REACT_APP_OAUTH2_FLOW_TYPE

//
//
export const accessToken = () =>
  async (dispatch, getState) => {
    const oauth2 = decode(window.location.search.substring(1))
    window.history.replaceState({}, document.title, '/oauth2/account')
    dispatch({ type: EXCHANGE })

    if (oauth2.error) {
      dispatch({ type: ERROR, reason: oauth2.error })
    } else if (oauth2.access_token) {
      const token = accessTokenImplicit(oauth2)
      dispatch({ ...token, type: SIGNIN })
    } else if (oauth2.code) {
      try {
        const token = await accessTokenExchange(oauth2)
        dispatch({ ...token, type: SIGNIN })
      } catch (e) {
        dispatch({ type: ERROR, reason: 'unauthorized' })
      }
    } else {
      const token = accessTokenStorage()
      dispatch({ ...token, type: SIGNIN })
    }
  }

const accessTokenStorage = () => {
  const token = window.localStorage.getItem('access_token')
  const expires = window.localStorage.getItem('access_token_ttl')
  return { token, expires }
}

const accessTokenImplicit = (oauth2) => {
  const now = +new Date()
  const token = `Bearer ${oauth2.access_token}`
  const expires = now + oauth2.expires_in * 1000 - 60000
  window.localStorage.setItem('access_token', token)
  window.localStorage.setItem('access_token_ttl', expires)
  return { token, expires }
}

const accessTokenExchange = async oauth2 => (
  fetch(OAUTH2_TOKEN, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: encode({ grant_type: 'authorization_code', client_id: OAUTH2_CLIENT_ID, code: oauth2.code })
  })
    .then(jsonify)
    .then(accessTokenImplicit)
)

const jsonify = (http) => {
  if (http.ok) {
    return http.json()
  }

  throw new Error({ type: `https://httpstatuses.com/${http.status}`, status: http.status })
}

//
//
export const requireAuth = () =>
  (dispatch, getState) => {
    const now = +new Date()
    const oauth2 = getState().oauth2
    return (!oauth2.exchange && oauth2.token === null) ||
         (oauth2.token !== null && oauth2.expires < now)
  }


//
//
export const authorize = () =>
  (dispatch, getState) => {
    window.localStorage.removeItem('access_token')
    window.localStorage.removeItem('access_token_ttl')
    window.location = `${OAUTH2_AUTHORIZE}/?${encode({ client_id: OAUTH2_CLIENT_ID, response_type: OAUTH2_FLOW_TYPE, state: 'none' })}`
    return ''
  }

//
//
export const withToken = f =>
  (dispatch, getState) => (f(getState().oauth2.token))


//
//
const empty = {
  token: null,
  expires: 0,
  exchange: false,
  error: null
}

export default (state = empty, action) => {
  switch (action.type) {
    case EXCHANGE:
      return { ...state, exchange: true }
    case ERROR:
      return { ...state, error: action.reason, exchange: false }
    case SIGNIN:
      return { ...state, token: action.token, expires: action.expires, exchange: false }
    default:
      return { ...state }
  }
}
