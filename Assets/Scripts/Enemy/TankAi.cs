using UnityEngine;

public class TankAi : EnemyAi
{

    public override void Update()
    {
        var position = transform.position;
        playerInSightRange = Physics.CheckSphere(position, sightRange, isPlayer);
        playerInAttackRange = Physics.CheckSphere(position, attackRange, isPlayer);
      
        if (!playerInSightRange && !playerInAttackRange) Patrolling();
        if (playerInSightRange && !playerInAttackRange) ChasePlayer();
    }

    public override void OnTriggerEnter(Collider other)
    {
        var target = other.GetComponent<IDamageable>();
        if (target == null) return;
        if (target.CanDamage())
        {
            TakeDamage(target.Damage());
        }
        else
        {
            target.TakeDamage(damage);
        }
    }
}