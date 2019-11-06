import React from 'react'
import { Button, Intent } from '@blueprintjs/core'
import { Link } from 'react-router-dom'
import { Dialog } from 'components/Dialog'
import { KeyPair } from 'components/KeyPair'

const prefix = process.env.REACT_APP_PREFIX || ''

const Actions = () => (
  <>
    <Link className="bp3-button bp3-minimal bp3-intent-primary" to="/">
      <b>Sign In Instead</b>
    </Link>
    <Button type="submit" intent={Intent.PRIMARY} large>Sign Up</Button>  
  </>
)

const SignUp = props => (
  <Dialog
    icon="fa-user-circle-o"
    title="Sign Up"
    url={`${prefix}/oauth2/signup`}
    Actions={Actions}
  >
    <KeyPair { ...props } />
  </Dialog>
)

export default SignUp
