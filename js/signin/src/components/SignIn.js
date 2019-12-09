import React from 'react'
import { Button, Intent } from '@blueprintjs/core'
import { Link } from 'react-router-dom'
import Dialog from 'components/Dialog'
import KeyPair from 'components/KeyPair'
// import { GitHub } from 'components/GitHub'

const prefix = process.env.REACT_APP_PREFIX || ''

const Actions = () => (
  <>
    <Link className="bp3-button bp3-minimal bp3-intent-primary" to="/signup">
      <b>Create Account</b>
    </Link>
    <Button type="submit" intent={Intent.PRIMARY} large>Sign In</Button>
  </>
)

/*
  TODO: Enable GitHub login feature
        <Dialog ... Links={Links} />
const Links = props => (
  <>
    {window.env.GITHUB &&
      <GitHub { ...props } />
    }
  </>
)
*/

const SignIn = ({ oauth2 }) => (
  <Dialog
    icon="fa-user-circle"
    title="Sign In"
    url={`${prefix}/oauth2/signin`}
    Actions={window.env.KEYPAIR ? Actions : null}
  >
    {window.env.KEYPAIR
      && <KeyPair oauth2={oauth2} />}
    {(window.env.KEYPAIR && window.env.KEYPAIR_RESET)
      && <Link className="bp3-button bp3-minimal bp3-intent-primary bp3-small" to="/reset">Forgot Password?</Link>}
  </Dialog>
)

export default SignIn
