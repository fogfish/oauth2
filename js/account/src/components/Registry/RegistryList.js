import React, { useState } from 'react'
import { Code, AnchorButton, Intent, Icon } from '@blueprintjs/core'
import { useSecureRemove, PENDING, SUCCESS, FAILURE, unknown } from '../OAuth2'

const Item = ({ app, access, security, redirect_uri }) => {
  // useSecureRemove hook allows to evaluate IO-effect sequentially
  // updateUrl causes its re-evaluation if url is changes
  // We need to handle gracefully degradation of IO
  // Intermediate failure state caches previous failure and 
  // resets the original IO to initial state
  const { status, url, updateUrl, updateStatus } = useSecureRemove(undefined)
  const [ failure, updateFailure ] = useState(status)
  if (status.status === FAILURE && url !== undefined) {
    updateFailure( status )
    updateStatus(unknown())
    updateUrl(undefined)
  }

  return (/*status.status === SUCCESS ? null :*/
    <tr>
      <td>{app}</td>
      <td><Code>{access}</Code></td>
      <td>{security}</td>
      <td>{redirect_uri}</td>
      <td>
        <AnchorButton
          intent={Intent.DANGER}
          minimal
          loading={status.status === PENDING}
          onClick={() => updateUrl(`https://pr15.auth.fog.fish/oauth2/client/${access}`)}
        >
          revoke
        </AnchorButton>
      </td>
      <td>
        {failure.status === FAILURE ? 
          <Icon intent={Intent.DANGER} icon='warning-sign' /> : 
          <Icon icon='' />
        }
      </td>
    </tr>
  )
}


const RegistryList = ({ registry, revoke }) => (
  <table className="bp3-html-table bp3-interactive" style={{width: '100%'}}>
    <thead>
      <tr>
        <th>Application</th>
        <th>Client ID</th>
        <th>Security</th>
        <th>Redirect Uri</th>
        <th></th>
        <th></th>
      </tr>
    </thead>
    <tbody>
      {registry.map((x) => <Item key={x.access} {...x} revoke={revoke} />)}
    </tbody>
  </table>
)

export default RegistryList
