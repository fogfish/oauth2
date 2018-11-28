import React from 'react'
import { Label, Input } from 'react-dress-code'

export const AccessKey = () => (
  <div className="dc-input-stack">
    <Label sub="required">Email</Label>
    <Input 
      id="access" 
      type="email"
      autoFocus
      required
    />
  </div>
)
